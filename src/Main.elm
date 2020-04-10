module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html as H exposing (Html, button, div, input, main_, nav, text)
import Html.Attributes as Attr exposing (class, classList, id, style, value)
import Html.Events as Ev exposing (onClick, onInput, stopPropagationOn)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Json.Decode as D
import Json.Decode.Pipeline as Dp
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
    , viewportPosition : Posix
    , viewportHeight : Int
    , viewportMillis : Int
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
    , at : Posix
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
    | Empty



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
      , viewportPosition = Time.millisToPosix 0
      , viewportHeight = 900
      , viewportMillis = 1000 * 60 * 60 * 24 -- 1 day
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform TickTime Time.now
        , Task.attempt
            (\result ->
                case result of
                    Ok el ->
                        ReadViewportHeight el

                    _ ->
                        Noop
            )
            (Dom.getElement "container")
        ]
    )



-- unwrapResult: Result a b -> c -> c b -> c
-- unwrapResult
-- ██╗   ██╗██████╗ ██████╗  █████╗ ████████╗███████╗
-- ██║   ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
-- ██║   ██║██████╔╝██║  ██║███████║   ██║   █████╗
-- ██║   ██║██╔═══╝ ██║  ██║██╔══██║   ██║   ██╔══╝
-- ╚██████╔╝██║     ██████╔╝██║  ██║   ██║   ███████╗
--  ╚═════╝ ╚═╝     ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝


type Msg
    = TickTime Posix
    | AdjustTimeZone Time.Zone
    | ReadViewportHeight Dom.Element
    | ClickOnFreeSpace
    | ClickOnLog LogID
    | InputTitle String
    | InputTitleKeyDown Int
    | DeleteLog LogID
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( TickTime posix, _ ) ->
            ( { model | currentTime = posix }, Cmd.none )

        ( AdjustTimeZone zone, _ ) ->
            ( { model | currentZone = zone }, Cmd.none )

        ( ReadViewportHeight element, _ ) ->
            ( { model | viewportHeight = round element.element.height }, Cmd.none )

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
            model
                |> updateMode Scrolling
                |> andBackupModel

        ( ClickOnLog logID, Scrolling ) ->
            ( { model | mode = Edit logID }
            , Task.attempt (\_ -> Noop) (Dom.focus "editing-log")
            )

        ( InputTitle text, Edit logID ) ->
            ( model
                |> updateLog logID (\l -> { l | title = text })
            , Cmd.none
            )

        ( InputTitleKeyDown key, Edit _ ) ->
            if key == 13 then
                model
                    |> updateMode Scrolling
                    |> andBackupModel

            else
                ( model, Cmd.none )

        ( DeleteLog logID, _ ) ->
            model
                |> updateLog logID (\l -> { l | category = Empty })
                |> consolidateEmptyLogs
                |> updateMode Scrolling
                |> andBackupModel

        ( _, _ ) ->
            ( model, Cmd.none )


andBackupModel : Model -> ( Model, Cmd Msg )
andBackupModel model =
    ( model, Ports.backupToLocalStorage (E.encode 0 (modelEncode model)) )


consolidateEmptyLogs : Model -> Model
consolidateEmptyLogs model =
    let
        allLogs =
            sortedLogs model.logs
    in
    case allLogs of
        _ :: rest ->
            model
                |> updateLogs
                    (rest
                        |> List.map2 detectUnnecesaryEmptyLog allLogs
                        |> List.filterMap identity
                        |> List.foldl (\log logs -> Dict.remove log.id logs) model.logs
                    )

        [] ->
            model


detectUnnecesaryEmptyLog : Log -> Log -> Maybe Log
detectUnnecesaryEmptyLog log1 log2 =
    if log1.category == Empty && log2.category == Empty then
        Just log2

    else
        Nothing


updateMode : Mode -> Model -> Model
updateMode mode model =
    { model | mode = mode }


updateLog : LogID -> (Log -> Log) -> Model -> Model
updateLog logID updateFun model =
    model
        |> updateLogs (model.logs |> Dict.update logID (Maybe.map updateFun))


updateLogs : Logs -> Model -> Model
updateLogs logs model =
    { model | logs = logs }


createNewLog : Model -> Log
createNewLog { logs, currentTime } =
    let
        lastLogAt =
            sortedLogs logs
                |> List.reverse
                |> List.head
                |> Maybe.map (\l -> sumPosix l.at (1000 * 60 * 60))
                |> Maybe.withDefault currentTime
    in
    { id = newID logs
    , title = "arsars"
    , category = Uncategorized
    , createdAt = currentTime
    , at = lastLogAt

    -- , endAt =
    --     currentTime
    --         |> Time.posixToMillis
    --         |> (+) (1000 * 60 * 60)
    --         |> Time.millisToPosix
    , tags = []
    , details = ""
    }


sortedLogs : Logs -> List Log
sortedLogs logs =
    Dict.values logs
        |> List.sortBy (\log -> Time.posixToMillis log.at)



-- snapLog : List Log -> Log -> Log
-- snapLog logs log =
--     case logs of
--         firstLog :: _ ->
--             log
--         [] ->
--             log
-- getSnapLocations : Logs -> List (Int, Int)
-- getSnapLocations logs =
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
        [ div [ c "container", id "container" ]
            [ nav [ c "nav" ] []
            , main_ [ c "main", onClick ClickOnFreeSpace ]
                [ lazy viewLogsList model
                ]
            ]
        ]
    }


type alias LogRenderConfig =
    { initialTime : Posix
    , pixelsPerHour : Int
    , timeZone : Time.Zone
    , mode : Mode
    }


viewViewport : Time.Posix -> Int -> Html Msg
viewViewport timePosition millisOnScreen =
    div [] []


type alias LogsRenderConfig a =
    { a
        | viewportMillis : Int
        , viewportHeight : Int
        , currentZone : Time.Zone
        , logs : Logs
        , mode : Mode
    }


viewLogsList : LogsRenderConfig a -> Html Msg
viewLogsList { viewportHeight, viewportMillis, currentZone, logs, mode } =
    let
        allLogs =
            sortedLogs logs
    in
    div [ c "logs" ]
        (case allLogs of
            firstLog :: restOfLogs ->
                let
                    logRenderConfig : LogRenderConfig
                    logRenderConfig =
                        { initialTime = firstLog.at
                        , pixelsPerHour = viewportHeight * (60 * 60 * 1000) // viewportMillis
                        , timeZone = currentZone
                        , mode = mode
                        }
                in
                allLogs
                    |> List.map (\log -> viewLog logRenderConfig log)
                    |> List.map2 (\next vlog -> vlog next) restOfLogs

            _ ->
                []
        )


viewLog : LogRenderConfig -> Log -> Log -> Html Msg
viewLog config log nextLog =
    viewLogPaint config log nextLog



-- case config.mode of
--     Scrolling ->
--         viewLogSimple config log
--     Edit logID ->
--         if log.id == logID then
--             viewLogEdit config log
--         else
--             viewLogSimple config log


viewLogPaint : LogRenderConfig -> Log -> Log -> Html Msg
viewLogPaint config log nextLog =
    div
        [ c ("log log__" ++ categoryToSlug log.category)
        , style "top" <|
            px (calculateTop config.initialTime log.at config.pixelsPerHour)
        , style "height" <|
            px (calculateHeight log.at nextLog.at config.pixelsPerHour)
        ]
        [ div
            [ c "log__box"
            , onClickUnpropagated (DeleteLog log.id)
            ]
            []
        ]


calculateTop : Posix -> Posix -> Int -> Int
calculateTop initialTime thisTime pixelsPerHour =
    timeDiff initialTime thisTime * pixelsPerHour // (1000 * 60 * 60)


calculateHeight : Posix -> Posix -> Int -> Int
calculateHeight at endAt pixelsPerHour =
    timeDiff at endAt * pixelsPerHour // (1000 * 60 * 60)


timeDiff : Posix -> Posix -> Int
timeDiff from to =
    Time.posixToMillis to - Time.posixToMillis from


posixToHourString : Time.Zone -> Posix -> String
posixToHourString timeZone posix =
    (Time.toHour timeZone posix
        |> String.fromInt
        |> String.padLeft 2 '0'
    )
        ++ ":"
        ++ (Time.toMinute timeZone posix
                |> String.fromInt
                |> String.padLeft 2 '0'
           )


viewLogSimple : LogRenderConfig -> Log -> Html Msg
viewLogSimple config log =
    div [ c ("log log__" ++ categoryToSlug log.category) ]
        [ div
            [ c "log__box"
            , onClickUnpropagated (ClickOnLog log.id)
            ]
            [ text (posixToHourString config.timeZone log.at)
            , text log.title
            ]
        ]


viewLogEdit : LogRenderConfig -> Log -> Html Msg
viewLogEdit config log =
    div [ c "log log--edit" ]
        [ div [ c "log__box", onClickUnpropagated Noop ]
            [ input
                [ value log.title
                , Attr.id "editing-log"
                , onInput InputTitle
                , onKeyDown InputTitleKeyDown
                ]
                []
            , button
                [ onClick (DeleteLog log.id)
                ]
                [ text "X" ]
            ]
        ]


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



-- ██╗  ██╗███████╗██╗     ██████╗ ███████╗██████╗ ███████╗
-- ██║  ██║██╔════╝██║     ██╔══██╗██╔════╝██╔══██╗██╔════╝
-- ███████║█████╗  ██║     ██████╔╝█████╗  ██████╔╝███████╗
-- ██╔══██║██╔══╝  ██║     ██╔═══╝ ██╔══╝  ██╔══██╗╚════██║
-- ██║  ██║███████╗███████╗██║     ███████╗██║  ██║███████║
-- ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝╚══════╝


px : Int -> String
px i =
    String.fromInt i ++ "px"


sumPosix : Posix -> Int -> Posix
sumPosix time add =
    Time.millisToPosix (Time.posixToMillis time + add)


onClickUnpropagated : msg -> H.Attribute msg
onClickUnpropagated msg =
    stopPropagationOn "click"
        (D.map (\m -> ( m, True )) (D.succeed msg))


onKeyDown : (Int -> msg) -> H.Attribute msg
onKeyDown tagger =
    Ev.on "keydown" (D.map tagger Ev.keyCode)



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
        , ( "at", posixEncoder log.at )
        , ( "tags", E.list E.string log.tags )
        , ( "details", E.string log.details )
        ]


logDecode : D.Decoder Log
logDecode =
    D.succeed Log
        |> Dp.required "id" D.string
        |> Dp.required "title" D.string
        |> Dp.required "category" categoryDecoder
        |> Dp.required "createdAt" posixDecoder
        |> Dp.required "at" posixDecoder
        |> Dp.required "tags" (D.list D.string)
        |> Dp.required "details" D.string


categoryEncoder : Category -> E.Value
categoryEncoder category =
    E.string (categoryToSlug category)


categoryToSlug : Category -> String
categoryToSlug category =
    case category of
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

        Empty ->
            "Empty"


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

                        "Uncategorized" ->
                            Uncategorized

                        _ ->
                            Empty
                    )
            )


posixEncoder : Posix -> E.Value
posixEncoder time =
    E.int (Time.posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))
