module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html as H exposing (Html, button, div, input, main_, nav, span, text)
import Html.Attributes as Attr exposing (class, classList, id, style, title, value)
import Html.Events as Ev exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)
import Json.Decode as D
import Json.Encode as E
import Log exposing (Log)
import Ports
import PosixExtra as PXE
import Task exposing (Task)
import Time exposing (Posix)
import Timeline exposing (Timeline)
import Viewport as VP exposing (Viewport)


fi : String -> D.Decoder a -> D.Decoder a
fi =
    D.field


c =
    class


cx =
    classList



------------------------------------ ████████╗██╗   ██╗██████╗ ███████╗███████╗
------------------------------------ ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝
------------------------------------    ██║    ╚████╔╝ ██████╔╝█████╗  ███████╗
------------------------------------    ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ╚════██║
------------------------------------    ██║      ██║   ██║     ███████╗███████║
------------------------------------    ╚═╝      ╚═╝   ╚═╝     ╚══════╝╚══════╝


type alias Logs =
    Timeline Log


type alias LogIndex =
    Int


type alias Model =
    { logs : Logs
    , mode : Mode
    , currentTime : Posix
    , currentZone : Time.Zone
    , viewport : Viewport
    , newLogDrag : DragStatus
    , snapMillis : Int
    }


type DragStatus
    = DragInactive
    | Dragging ( Posix, Posix )


type alias ModelBackup =
    { logs : Logs
    }


type Mode
    = Scrolling
    | Edit LogIndex



------------------------------------------------------ ██╗███╗   ██╗██╗████████╗
------------------------------------------------------ ██║████╗  ██║██║╚══██╔══╝
------------------------------------------------------ ██║██╔██╗ ██║██║   ██║
------------------------------------------------------ ██║██║╚██╗██║██║   ██║
------------------------------------------------------ ██║██║ ╚████║██║   ██║
------------------------------------------------------ ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝
-- type InitialData = {
--     modelBackup: ModelBackup
--     , viewportHeight:
-- }


init : D.Value -> ( Model, Cmd Msg )
init localStorageData =
    let
        initialLogs =
            case D.decodeValue modelDecode localStorageData of
                Ok modelBackup ->
                    modelBackup.logs

                Err errorMsg ->
                    Debug.log (D.errorToString errorMsg) Timeline.empty
    in
    ( { logs = initialLogs
      , mode = Scrolling
      , currentTime = Time.millisToPosix 0
      , currentZone = Time.utc
      , viewport =
            { firstDate = Time.millisToPosix 1586401200000 -- 2020-04-09
            , lastDate = Time.millisToPosix 1586660400000 -- 2020-04-12
            , visibleTimespan = 1000 * 60 * 60 * 12
            , scroll = 0
            , height = 986
            }
      , newLogDrag = DragInactive
      , snapMillis = 1000 * 60 * 15 -- 15min
      }
    , Task.attempt (Result.withDefault Noop) initialTask
    )


initialTask : Task Dom.Error Msg
initialTask =
    Dom.getElement "container"
        |> Task.andThen
            (\el ->
                Task.map2
                    (\zone posix -> InitializationTask ( el, zone, posix ))
                    Time.here
                    Time.now
            )



---------------------------- ██╗   ██╗██████╗ ██████╗  █████╗ ████████╗███████╗
---------------------------- ██║   ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
---------------------------- ██║   ██║██████╔╝██║  ██║███████║   ██║   █████╗
---------------------------- ██║   ██║██╔═══╝ ██║  ██║██╔══██║   ██║   ██╔══╝
---------------------------- ╚██████╔╝██║     ██████╔╝██║  ██║   ██║   ███████╗
----------------------------  ╚═════╝ ╚═╝     ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝


type Msg
    = InitializationTask ( Dom.Element, Time.Zone, Posix )
    | TickTime Posix
    | OnScroll Int
    | Splitting SplittingMsg Int
    | ClickOnLog LogIndex
    | InputTitle LogIndex String
    | SetCategory LogIndex (Maybe Log.Category)
    | InputTitleKeyDown Int
    | DeleteLog LogIndex
    | Noop


type SplittingMsg
    = SplitStart
    | SplitProgress
    | SplitEnd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( InitializationTask ( element, zone, posix ), _ ) ->
            { model | currentZone = zone, currentTime = posix }
                |> updateViewport
                    -- Hardcoded nav height = 80
                    (\v -> { v | height = round element.element.height - 80 })
                |> setViewportBoundaries
                |> andScrollViewportToNow

        ( TickTime posix, _ ) ->
            ( { model | currentTime = posix }, Cmd.none )

        ( OnScroll scrollPos, _ ) ->
            model
                |> updateViewport (\v -> { v | scroll = scrollPos })
                -- |> andDebug (\m -> Debug.log "ReceivedScroll" scrollPos)
                |> andCmd Cmd.none

        ( Splitting splitMsg posY, _ ) ->
            let
                _ =
                    ""

                --Debug.log "Split msg " posY
            in
            case ( splitMsg, model.newLogDrag ) of
                ( SplitStart, DragInactive ) ->
                    model
                        |> updateDragStatus posY
                        |> andCmd Cmd.none

                ( SplitProgress, Dragging _ ) ->
                    model
                        |> updateDragStatus posY
                        |> andCmd Cmd.none

                ( SplitEnd, Dragging _ ) ->
                    model
                        |> addLogFromDrag
                        |> updateDragStatus -1
                        |> andBackupModel

                ( _, _ ) ->
                    ( model, Cmd.none )

        -- ( ClickOnFreeSpace, Edit _ ) ->
        --     model
        --         |> updateMode Scrolling
        --         |> andBackupModel
        ( ClickOnLog index, Scrolling ) ->
            ( { model | mode = Edit index }
            , Task.attempt (\_ -> Noop) (Dom.focus "editing-log")
            )

        ( InputTitle index text, _ ) ->
            model
                |> updateLog index (\l -> { l | title = text })
                |> andBackupModel

        ( SetCategory index category, _ ) ->
            model
                |> updateLog index (\l -> { l | category = category })
                |> andBackupModel

        ( InputTitleKeyDown key, _ ) ->
            if key == 13 then
                model
                    |> updateMode Scrolling
                    |> andBackupModel

            else
                ( model, Cmd.none )

        ( DeleteLog index, _ ) ->
            model
                |> updateLogs (Timeline.remove index model.logs)
                |> andBackupModel

        ( _, _ ) ->
            ( model, Cmd.none )


andDebug : (Model -> a) -> Model -> Model
andDebug fun model =
    let
        _ =
            fun model
    in
    model


andBackupModel : Model -> ( Model, Cmd Msg )
andBackupModel model =
    ( model, Ports.backupToLocalStorage (E.encode 0 (modelEncode model)) )


updateDragStatus : Int -> Model -> Model
updateDragStatus clientY model =
    if clientY == -1 then
        { model | newLogDrag = DragInactive }

    else
        let
            posix =
                clientY
                    |> (+) model.viewport.scroll
                    |> VP.pxToMillis model.viewport
                    |> snapBy model.snapMillis
                    |> Time.millisToPosix
        in
        { model
            | newLogDrag =
                case model.newLogDrag of
                    DragInactive ->
                        Dragging ( posix, posix )

                    Dragging ( first, second ) ->
                        Dragging ( first, posix )
        }


snapBy : Int -> Int -> Int
snapBy snap num =
    num - modBy snap num


andCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
andCmd cmd model =
    ( model, cmd )


updateViewport : (Viewport -> Viewport) -> Model -> Model
updateViewport updateFun model =
    { model | viewport = updateFun model.viewport }


setViewportBoundaries : Model -> Model
setViewportBoundaries model =
    model
        |> updateViewport
            (\v ->
                { v
                    | firstDate = PXE.add model.currentTime -(1000 * 60 * 60 * 24 * 5)
                    , lastDate = PXE.add model.currentTime (1000 * 60 * 60 * 24 * 5)
                }
            )


andScrollViewportToNow : Model -> ( Model, Cmd Msg )
andScrollViewportToNow model =
    ( model
    , Ports.scrollViewportTo <|
        VP.millisToPx
            model.viewport
            (PXE.diff model.viewport.firstDate model.currentTime)
    )


updateMode : Mode -> Model -> Model
updateMode mode model =
    { model | mode = mode }


updateLogs : Logs -> Model -> Model
updateLogs logs model =
    { model | logs = logs }


updateLog : LogIndex -> (Log -> Log) -> Model -> Model
updateLog index updateFun model =
    { model | logs = model.logs |> Timeline.update index updateFun }


addLogFromDrag : Model -> Model
addLogFromDrag model =
    case model.newLogDrag of
        Dragging ( start, end ) ->
            model
                |> updateLogs
                    (model.logs
                        |> Timeline.add Log.empty start end (\l -> l.title == "") (1000 * 60 * 60)
                    )

        DragInactive ->
            model



---------------------------------------- ██╗   ██╗██╗███████╗██╗    ██╗███████╗
---------------------------------------- ██║   ██║██║██╔════╝██║    ██║██╔════╝
---------------------------------------- ██║   ██║██║█████╗  ██║ █╗ ██║███████╗
---------------------------------------- ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║╚════██║
----------------------------------------  ╚████╔╝ ██║███████╗╚███╔███╔╝███████║
----------------------------------------   ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝ ╚══════╝


view : Model -> Document Msg
view model =
    { title = "The Hero Journey: The Story Of Your Life"
    , body =
        [ div [ c "container", id "container" ]
            [ nav [ c "nav" ] []
            , main_ [ c "main" ]
                [ lazy4 viewViewport
                    model.viewport
                    model.newLogDrag
                    model.currentZone
                    model.logs
                ]
            ]
        , viewDebug model
        ]
    }


viewViewport : Viewport -> DragStatus -> Time.Zone -> Logs -> Html Msg
viewViewport viewport newLogDrag timeZone logs =
    div
        [ c "viewport"
        , id "viewport"
        , cx [ ( "viewport--dragging", newLogDrag /= DragInactive ) ]
        , onScroll OnScroll
        , onSplitStart Splitting
        , onSplitProgress (newLogDrag /= DragInactive) Splitting
        , onSplitEnd (newLogDrag /= DragInactive) Splitting
        ]
        [ div
            [ c "logs"
            , style "height" (px (VP.fullHeight viewport))
            ]
            [ lazy3 viewLogsList viewport timeZone logs
            , div (c "viewport-gradient" :: VP.repeatingGradient viewport timeZone) []
            , case newLogDrag of
                Dragging ( start, end ) ->
                    viewLogGhost viewport start end

                DragInactive ->
                    div [] []
            ]
        ]


viewLogGhost : Viewport -> Posix -> Posix -> Html Msg
viewLogGhost viewport start end =
    let
        ( top, bottom ) =
            PXE.sort2 ( start, end )
    in
    div
        [ c "log log--Ghost"
        , style "top" <| VP.posixToPxFloat viewport top
        , style "height" <| VP.millisToPxFloat viewport (PXE.diff top bottom)
        ]
        [ div [ c "log__box" ] [] ]


viewLogsList : Viewport -> Time.Zone -> Logs -> Html Msg
viewLogsList viewport zone logs =
    Keyed.node "div"
        []
        (logs
            |> Timeline.map
                (\index log nextLog ->
                    viewLogPaint viewport zone index log nextLog
                )
        )


viewLogPaint : Viewport -> Time.Zone -> LogIndex -> Maybe Log -> ( Posix, Posix ) -> ( String, Html Msg )
viewLogPaint viewport zone index maybeLog ( top, bottom ) =
    let
        timespan =
            PXE.diff top bottom

        id =
            top
                |> Time.posixToMillis
                |> String.fromInt
    in
    case maybeLog of
        Nothing ->
            ( id
            , viewLogPaintEmpty
                viewport
                zone
                top
                timespan
            )

        Just log ->
            ( id
            , viewLogPaintLogd
                viewport
                zone
                top
                timespan
                index
                log
            )


viewLogPaintEmpty : Viewport -> Time.Zone -> Posix -> Int -> Html Msg
viewLogPaintEmpty vp zone at timespan =
    div [ c "log log--Empty", topStyle vp at, heightStyle vp timespan ]
        [ viewLogTime zone at ]


viewLogPaintLogd : Viewport -> Time.Zone -> Posix -> Int -> Int -> Log -> Html Msg
viewLogPaintLogd vp zone at timespan index log =
    div
        [ c ("log log--" ++ Log.categoryToSlug log.category)
        , topStyle vp at
        , heightStyle vp timespan
        , viewLogBoxSizeClass timespan
        ]
        [ viewLogTime zone at
        , lazy2 viewLogBox index log
        ]


topStyle : Viewport -> Posix -> H.Attribute Msg
topStyle vp top =
    style "top" <| px (VP.posixToPx vp top)


heightStyle : Viewport -> Int -> H.Attribute Msg
heightStyle vp timespan =
    style "height" <| px (VP.millisToPx vp timespan)


viewLogBoxSizeClass : Int -> H.Attribute Msg
viewLogBoxSizeClass timespan =
    if timespan <= (1000 * 60 * 10) then
        -- 10min
        c "log--10min log--30min log--60min"

    else if timespan <= (1000 * 60 * 30) then
        -- 30min
        c "log--30min log--60min"

    else if timespan <= (1000 * 60 * 60) then
        -- 1 hour
        c "log--60min"

    else
        c ""


viewLogTime : Time.Zone -> Posix -> Html Msg
viewLogTime zone posix =
    div [ c "log__time" ] [ text (PXE.toNormalTime zone posix) ]


viewLogBox : LogIndex -> Log -> Html Msg
viewLogBox index log =
    div [ c "log__box" ]
        [ div [ c "log__category", noPropagation "mousedown" ]
            [ viewLogBoxCatSelect "Self care" log.category Log.SelfCare (SetCategory index)
            , viewLogBoxCatSelect "Re creative" log.category Log.Recreative (SetCategory index)
            , viewLogBoxCatSelect "Creative" log.category Log.Creative (SetCategory index)
            , viewLogBoxCatSelect "Self growth" log.category Log.SelfGrowth (SetCategory index)
            ]

        --  else
        --     [ div [ onClick (SetCategory index Nothing) ] [] ]
        -- )
        , div
            [ c "log__title" ]
            [ input
                [ c "log__titleInput"
                , noPropagation "mousedown"
                , onInput (InputTitle index)
                , value log.title
                ]
                []

            -- , div [] [ text log.id ]
            ]
        , button
            [ c "log__delete"
            , noPropagation "mousedown"
            , onClick (DeleteLog index)
            ]
            [ text "×" ]
        ]


viewLogBoxCatSelect : String -> Maybe Log.Category -> Log.Category -> (Maybe Log.Category -> msg) -> Html msg
viewLogBoxCatSelect name maybeSelected category msg =
    let
        isSelected =
            case maybeSelected of
                Just selected ->
                    selected == category

                Nothing ->
                    False
    in
    div
        [ c ("log__categorySelect log__categorySelect--" ++ Log.categoryToSlug (Just category))
        , cx [ ( "log__categorySelect--selected", isSelected ) ]
        , title name
        , onClick
            (msg
                (if isSelected then
                    Nothing

                 else
                    Just category
                )
            )
        ]
        [ span [] [ text name ] ]


viewDebug : Model -> Html Msg
viewDebug ({ viewport, currentZone } as model) =
    div [ c "debug" ]
        [ div [] [ text ("Scroll: " ++ String.fromInt viewport.scroll) ]
        , viewDebugPosix "VP first date: " currentZone viewport.firstDate
        , viewDebugPosix "VP start date: " currentZone (VP.startDate viewport)
        , viewDebugPosix "VP end date: " currentZone (VP.endDate viewport)
        , viewDebugPosix "VP last date: " currentZone viewport.lastDate
        , case model.newLogDrag of
            Dragging ( first, second ) ->
                div []
                    [ viewDebugPosix "Drag From: " currentZone first
                    , viewDebugPosix "Drag To: " currentZone second
                    ]

            DragInactive ->
                div [] []
        ]


viewDebugPosix : String -> Time.Zone -> Posix -> Html Msg
viewDebugPosix labl zone posix =
    div []
        [ text (labl ++ PXE.toNormalDateTime zone posix)
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
        , subscriptions = \_ -> Sub.none
        }



---------------------- ██╗  ██╗███████╗██╗     ██████╗ ███████╗██████╗ ███████╗
---------------------- ██║  ██║██╔════╝██║     ██╔══██╗██╔════╝██╔══██╗██╔════╝
---------------------- ███████║█████╗  ██║     ██████╔╝█████╗  ██████╔╝███████╗
---------------------- ██╔══██║██╔══╝  ██║     ██╔═══╝ ██╔══╝  ██╔══██╗╚════██║
---------------------- ██║  ██║███████╗███████╗██║     ███████╗██║  ██║███████║
---------------------- ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝╚══════╝


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


noPropagation : String -> H.Attribute Msg
noPropagation event =
    stopPropagationOn event (D.map (\m -> ( m, True )) (D.succeed Noop))


onKeyDown : (Int -> msg) -> H.Attribute msg
onKeyDown tagger =
    Ev.on "keydown" (D.map tagger Ev.keyCode)


onSplitStart : (SplittingMsg -> Int -> msg) -> H.Attribute msg
onSplitStart msg =
    Ev.on "mousedown" <|
        D.map (msg SplitStart)
            (fi "clientY" D.int)



-- loggingDecoder <|
--     D.map (msg SplitStart) (fi "offsetY" D.int)


onSplitProgress : Bool -> (SplittingMsg -> Int -> msg) -> H.Attribute msg
onSplitProgress splitting msg =
    Ev.on "mousemove" <|
        rejectDecoderIf (not splitting) <|
            loggingDecoder <|
                D.map (msg SplitProgress) (fi "clientY" D.int)


onSplitEnd : Bool -> (SplittingMsg -> Int -> msg) -> H.Attribute msg
onSplitEnd splitting msg =
    Ev.on "mouseup" <|
        rejectDecoderIf (not splitting) <|
            D.map (msg SplitEnd) (fi "clientY" D.int)


rejectDecoderIf : Bool -> D.Decoder msg -> D.Decoder msg
rejectDecoderIf condition decoder =
    if condition then
        D.value
            |> D.andThen (\_ -> D.fail "Decoding aborted")

    else
        decoder



-- transverse up with offsetParent summing offsetTop until (id === "viewport")


viewportYDecoder : D.Decoder a -> D.Decoder a
viewportYDecoder decoder =
    decoder


onScroll : (Int -> msg) -> H.Attribute msg
onScroll msg =
    Ev.on "scroll" (D.map msg (fi "target" (fi "scrollTop" D.int)))


onTouchStart : (TouchEvent -> msg) -> H.Attribute msg
onTouchStart msg =
    Ev.on "touchstart" (D.map msg (fi "touches" (D.list mousePositionDecoder)))


type alias TouchEvent =
    List Point


type alias Point =
    { x : Int, y : Int }


mousePositionDecoder : D.Decoder Point
mousePositionDecoder =
    D.map2 Point (fi "clientX" D.int) (fi "clientY" D.int)


loggingDecoder : D.Decoder a -> D.Decoder a
loggingDecoder realDecoder =
    D.value
        |> D.andThen
            (\event ->
                let
                    _ =
                        ""

                    -- Debug.log "Ev" (E.encode 2 event)
                in
                case D.decodeValue realDecoder event of
                    Ok decoded ->
                        D.succeed decoded

                    Err error ->
                        error
                            |> D.errorToString
                            |> Debug.log "decoding error"
                            |> D.fail
            )



------------ ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
------------ ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
------------ █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
------------ ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
------------ ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
------------ ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


modelEncode : Model -> E.Value
modelEncode model =
    E.object [ ( "logs", Timeline.encoder Log.encoder model.logs ) ]


modelDecode : D.Decoder ModelBackup
modelDecode =
    D.map ModelBackup (fi "logs" (Timeline.decoder Log.decoder))
