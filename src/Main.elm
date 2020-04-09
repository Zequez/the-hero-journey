module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom
import Dict exposing (Dict)
import Html as H exposing (Html, button, div, input, main_, nav, text)
import Html.Attributes as Attr exposing (class, classList, value)
import Html.Events as Ev exposing (onClick, onInput, stopPropagationOn)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Json.Decode as D
import Json.Encode as E
import Ports
import Task exposing (Task)
import Time exposing (Posix)


c =
    class


cx =
    classList



-- ████████╗██╗   ██╗██████╗ ███████╗███████╗
-- ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝
--    ██║    ╚████╔╝ ██████╔╝█████╗  ███████╗
--    ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ╚════██║
--    ██║      ██║   ██║     ███████╗███████║
--    ╚═╝      ╚═╝   ╚═╝     ╚══════╝╚══════╝


type alias Model =
    { logs : Logs
    , mode : Mode
    , currentTime : Posix
    , currentZone : Time.Zone
    }


type alias ModelBackup =
    { logs : Logs
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
    | Uncategorized



-- ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
-- ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
-- █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
-- ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
-- ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
-- ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


modelEncode : Model -> E.Value
modelEncode model =
    E.object [ ( "logs", logsEncode model.logs ) ]


modelDecode : D.Decoder ModelBackup
modelDecode =
    D.map ModelBackup
        (D.field "logs" logsDecode)


logsEncode : Logs -> E.Value
logsEncode =
    E.dict identity logEncode


logsDecode : D.Decoder Logs
logsDecode =
    D.dict logDecode


logEncode : Log -> E.Value
logEncode log =
    E.object
        [ ( "id", E.string log.id )
        , ( "title", E.string log.title )
        , ( "category", categoryEncoder log.category )
        , ( "createdAt", posixEncoder log.createdAt )
        , ( "startAt", posixEncoder log.startAt )
        , ( "endAt", posixEncoder log.endAt )
        , ( "tags", E.list E.string log.tags )
        , ( "details", E.string log.details )
        ]


logDecode : D.Decoder Log
logDecode =
    D.map8 Log
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "category" categoryDecoder)
        (D.field "createdAt" posixDecoder)
        (D.field "startAt" posixDecoder)
        (D.field "endAt" posixDecoder)
        (D.field "tags" (D.list D.string))
        (D.field "details" D.string)


categoryEncoder : Category -> E.Value
categoryEncoder category =
    E.string
        (case category of
            SelfCare ->
                "SelfCare"

            Recreative ->
                "Recreative"

            Creative ->
                "Creative"

            SelfGrowth ->
                "SelfGrowth"

            Uncategorized ->
                "Uncategorized"
        )


categoryDecoder : D.Decoder Category
categoryDecoder =
    D.string
        |> D.andThen
            (\str ->
                D.succeed
                    (case str of
                        "SelfCare" ->
                            SelfCare

                        "Recreative" ->
                            Recreative

                        "Creative" ->
                            Creative

                        "SelfGrowth" ->
                            SelfGrowth

                        _ ->
                            Uncategorized
                    )
            )


posixEncoder : Posix -> E.Value
posixEncoder time =
    E.int (Time.posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))



-- ██╗   ██╗██████╗ ██████╗  █████╗ ████████╗███████╗
-- ██║   ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
-- ██║   ██║██████╔╝██║  ██║███████║   ██║   █████╗
-- ██║   ██║██╔═══╝ ██║  ██║██╔══██║   ██║   ██╔══╝
-- ╚██████╔╝██║     ██████╔╝██║  ██║   ██║   ███████╗
--  ╚═════╝ ╚═╝     ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝


type Msg
    = TickTime Posix
    | AdjustTimeZone Time.Zone
    | ClickOnFreeSpace
    | ClickOnLog LogID
    | InputTitle String
    | InputTitleKeyDown Int
    | ClickDelete
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
            { model
                | logs = model.logs |> Dict.insert newLog.id newLog
            }
                |> andBackupModel

        ( ClickOnFreeSpace, Edit _ ) ->
            { model | mode = Scrolling } |> andBackupModel

        ( ClickOnLog logID, Scrolling ) ->
            ( { model | mode = Edit logID }
            , Task.attempt (\_ -> Noop) (Browser.Dom.focus "editing-log")
            )

        ( InputTitle text, Edit logID ) ->
            ( model |> updateLog logID (\l -> { l | title = text })
            , Cmd.none
            )

        ( InputTitleKeyDown key, Edit _ ) ->
            if key == 13 then
                { model | mode = Scrolling }
                    |> andBackupModel

            else
                ( model, Cmd.none )

        ( ClickDelete, Edit logID ) ->
            { model
                | logs = model.logs |> Dict.remove logID
                , mode = Scrolling
            }
                |> andBackupModel

        ( _, _ ) ->
            ( model, Cmd.none )


andBackupModel : Model -> ( Model, Cmd Msg )
andBackupModel model =
    ( model, Ports.backupToLocalStorage (E.encode 0 (modelEncode model)) )


updateLog : LogID -> (Log -> Log) -> Model -> Model
updateLog logID updateFun model =
    { model | logs = model.logs |> Dict.update logID (Maybe.map updateFun) }


createNewLog : Model -> Log
createNewLog { logs, currentTime } =
    { id = newID logs
    , title = "arsars"
    , category = Uncategorized
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



-- ██╗   ██╗██╗███████╗██╗    ██╗███████╗
-- ██║   ██║██║██╔════╝██║    ██║██╔════╝
-- ██║   ██║██║█████╗  ██║ █╗ ██║███████╗
-- ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║╚════██║
--  ╚████╔╝ ██║███████╗╚███╔███╔╝███████║
--   ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝ ╚══════╝


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


onClickUnpropagated : msg -> H.Attribute msg
onClickUnpropagated msg =
    stopPropagationOn "click"
        (D.map (\m -> ( m, True )) (D.succeed msg))


onKeyDown : (Int -> msg) -> H.Attribute msg
onKeyDown tagger =
    Ev.on "keydown" (D.map tagger Ev.keyCode)


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
                , Attr.id "editing-log"
                , onInput InputTitle
                , onKeyDown InputTitleKeyDown
                ]
                []
            , button
                [ onClick ClickDelete
                ]
                [ text "X" ]
            ]
        ]



-- ██╗███╗   ██╗██╗████████╗
-- ██║████╗  ██║██║╚══██╔══╝
-- ██║██╔██╗ ██║██║   ██║
-- ██║██║╚██╗██║██║   ██║
-- ██║██║ ╚████║██║   ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


init : D.Value -> ( Model, Cmd Msg )
init localStorageData =
    let
        initialLogs =
            case D.decodeValue modelDecode localStorageData of
                Ok modelBackup ->
                    modelBackup.logs

                Err errorMsg ->
                    Debug.log (D.errorToString errorMsg) (Dict.fromList [])
    in
    ( { logs = initialLogs
      , mode = Scrolling
      , currentTime = Time.millisToPosix 0
      , currentZone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 TickTime


main : Program D.Value Model Msg
main =
    Browser.document
        { init = \localStorageData -> init localStorageData
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
