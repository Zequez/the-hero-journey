module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, main_, nav, text)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Json.Decode
import Ports
import Task exposing (Task)
import Time exposing (Posix)


c =
    class


cx =
    classList


type alias Model =
    { logs : Logs
    , mode : Mode
    , currentTime : Posix
    , currentZone : Time.Zone
    }


type alias Logs =
    Dict String Log


type Mode
    = Scrolling
    | Edit LogID


type alias LogID =
    String


type alias Log =
    { id : LogID
    , title : String
    , category : Category
    , createdAt : Posix
    , startAt : Posix
    , endAt : Posix
    , tags : List String
    , details : String
    }


newID : Logs -> LogID
newID logs =
    String.fromInt
        (1
            + (Dict.values logs
                |> List.map .id
                |> List.map (\str -> Maybe.withDefault 0 (String.toInt str))
                |> List.maximum
                |> Maybe.withDefault 0
              )
        )


type Category
    = SelfCare
    | Recreative
    | Creative
    | SelfGrowth
    | UnCategorized


type Msg
    = TickTime Posix
    | AdjustTimeZone Time.Zone
    | ClickOnFreeSpace
    | ClickOnLog LogID
    | InputTitle String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( TickTime posix, _ ) ->
            ( { model | currentTime = posix }, Cmd.none )

        ( AdjustTimeZone zone, _ ) ->
            ( { model | currentZone = zone }, Cmd.none )

        ( ClickOnFreeSpace, Scrolling ) ->
            let
                newLog =
                    createNewLog model
            in
            ( { model
                | logs = model.logs |> Dict.insert newLog.id newLog
              }
            , Ports.backupToLocalStorage "Potato"
            )

        ( ClickOnFreeSpace, Edit _ ) ->
            ( { model | mode = Scrolling }, Cmd.none )

        ( ClickOnLog logID, Scrolling ) ->
            ( { model | mode = Edit logID }, Cmd.none )

        ( InputTitle text, Edit logID ) ->
            ( model |> updateLog logID (\l -> { l | title = text })
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


updateLog : LogID -> (Log -> Log) -> Model -> Model
updateLog logID updateFun model =
    { model | logs = model.logs |> Dict.update logID (Maybe.map updateFun) }


createNewLog : Model -> Log
createNewLog { logs, currentTime } =
    { id = newID logs
    , title = "arsars"
    , category = UnCategorized
    , createdAt = currentTime
    , startAt = currentTime
    , endAt =
        currentTime
            |> Time.posixToMillis
            |> (+) (1000 * 60 * 60)
            |> Time.millisToPosix
    , tags = []
    , details = ""
    }


init : ( Model, Cmd Msg )
init =
    ( { logs = Dict.fromList []
      , mode = Scrolling
      , currentTime = Time.millisToPosix 0
      , currentZone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


view : Model -> Document Msg
view model =
    { title = "The Hero Journey: The Story Of Your Life"
    , body =
        [ div [ c "container" ]
            [ nav [ c "nav" ] []
            , main_ [ c "main", onClick ClickOnFreeSpace ]
                [ lazy2 viewLogsList model.logs model.mode
                ]
            ]
        ]
    }



-- viewViewport : List Log -> Html Msg
-- viewViewport logs =


viewLogsList : Logs -> Mode -> Html Msg
viewLogsList logs mode =
    div [ c (Debug.log "Rendering logs" "logs") ]
        (Dict.values logs
            |> List.map (\log -> viewLog log mode)
        )


viewLog : Log -> Mode -> Html Msg
viewLog log mode =
    case mode of
        Scrolling ->
            viewLogSimple log

        Edit logID ->
            if log.id == logID then
                viewLogEdit log

            else
                viewLogSimple log


onClickUnpropagated : msg -> Html.Attribute msg
onClickUnpropagated msg =
    stopPropagationOn "click"
        (Json.Decode.map (\m -> ( m, True )) (Json.Decode.succeed msg))


viewLogSimple : Log -> Html Msg
viewLogSimple log =
    div [ c "log" ]
        [ div
            [ c "log-box"
            , onClickUnpropagated (ClickOnLog log.id)
            ]
            [ text log.id
            , text log.title
            ]
        ]


viewLogEdit : Log -> Html Msg
viewLogEdit log =
    div [ c "log log-edit" ]
        [ div [ c "log-box", onClickUnpropagated Noop ]
            [ input
                [ value log.title
                , onInput InputTitle
                ]
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 TickTime


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
