module TheHeroJourney.Main exposing (Model, Msg, main, view)

import Browser
import Debug exposing (log)
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on)
import Json.Decode as JD
import Time exposing (Posix, Zone, millisToPosix, posixToMillis)


type RepeatPattern
    = None
    | Daily


type TimeBox
    = TimeBox
        { realizationTime : Posix -- Time of box
        , intentionTime : Posix -- Time of intention
        , length : Int -- In seconds
        , what : String -- What's in the box?
        , color : String -- What's the color of the box?
        , deleted : Bool -- What we delete we just hide
        , repeatPattern : RepeatPattern
        , backLinked : Maybe TimeBox
        }



-- type alias TimeMap =
--     { start : Posix
--     , boxes : List TimeBox
--     , length : Int
--     }


type alias Viewport =
    { at : Posix
    , v : Int
    , -- Vertical in millis
      h : Int -- Horizontal in millis
    }


type alias Model =
    { now : Posix
    , zone : Zone
    , focus : Posix

    -- , timeMap : TimeMap
    , allBoxes : List TimeBox
    , viewport : Viewport
    }


type alias Flags =
    {}


type Msg
    = CenterViewport Int Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> init flags
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init shared =
    ( { now = millisToPosix 0
      , zone = Time.utc
      , focus = millisToPosix 0

      --   , timeMap = {
      --       start = millisToPosix 0
      --     , boxes = []
      --     , length = 0
      --   }
      , allBoxes = []
      , viewport =
            { at = millisToPosix (-(1000 * 60 * 15) + (1000 * 60 * 60 * 4))
            , v = 1000 * 60 * 60 * 8 -- 8 hours
            , h = 1000 * 60 * 60 * 24 * 7 -- 7 Days
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CenterViewport scrolledX scrolledY ->
            let
                _ =
                    Debug.log "CenterViewport" scrolledY
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "bg-gray-300 overflow-auto"
        , style "width" "100vw"
        , style "height" "100vh"
        , onScroll CenterViewport
        ]
        [ div
            [ style "width" "300vw"
            , style "height" "300vh"
            ]
            [ div [ class "flex", style "transform" "translate(10px, 10px)" ]
                [ div [ class "w-12 bg-gray-200 h-full" ]
                    [ hoursRingView model.viewport.at model.zone model.viewport.v
                    ]
                , div [ class "" ]
                    [ text "Hello, world!"
                    ]
                ]
            ]
        ]


onScroll : (Int -> Int -> msg) -> Attribute msg
onScroll scrollMsg =
    on "wheel" (decodeScroll scrollMsg)


decodeScroll : (Int -> Int -> msg) -> JD.Decoder msg
decodeScroll scrollMsg =
    JD.succeed (scrollMsg 10 0)


hourBrightness : Int -> Int -> Int
hourBrightness hour timeOfYear =
    50



-- type HourMarker = {
--     posix: Posix
-- }


hoursRingView : Posix -> Zone -> Int -> Html Msg
hoursRingView at zone timespan =
    let
        startAt : Posix
        startAt =
            startingHour at zone

        hours : List Posix
        hours =
            round (toFloat timespan / 1000 / 60 / 60)
                |> List.range 0
                |> List.map (\h -> millisToPosix (posixToMillis startAt + h * 3600 * 1000))

        hourSpan : Float
        hourSpan =
            ((1000 * 60 * 60) / toFloat timespan) * 100

        -- _ =
        --     Debug.log "startAt" startAt
        -- _ =
        --     Debug.log "startAt" startAt
        -- positions : List Float
        -- positions =
        --     hours
        --         |> List.map
        --             (\posix ->
        --                 toFloat (posixToMillis posix - posixToMillis at) / toFloat timespan * 100
        --             )
        -- _ =
        --     Debug.log "Hours" hours
        -- a =
        --     Debug.log "Positions" positions
    in
    div [ class "relative h-full overflow-hidden" ]
        (hours
            |> List.map
                (\h ->
                    let
                        pos =
                            positionFromPosix at h timespan
                    in
                    hourMarkerView (Time.toHour zone h) pos hourSpan
                )
        )



-- at + timespan = 100%
-- at = 0%


positionFromPosix : Posix -> Posix -> Int -> Float
positionFromPosix start current timespan =
    toFloat (posixToMillis current - posixToMillis start) / toFloat timespan * 100



-- [ text (String.fromInt (Time.toHour zone at))
-- ]


hourMarkerView : Int -> Float -> Float -> Html Msg
hourMarkerView hour position height =
    -- let
    --     a =
    --         Debug.log "Hour" hour
    --     b =
    --         Debug.log "Pos" position
    -- in
    div
        [ class """
            absolute w-full text-center flex items-center text-xs
            justify-center border-t border-yellow-500 bg-yellow-300 text-white
        """
        , style "top" (String.fromFloat position ++ "%")
        , style "height" (String.fromFloat height ++ "%")
        ]
        [ text (String.padLeft 2 '0' (String.fromInt hour) ++ ":00")
        ]


startingHour : Posix -> Zone -> Posix
startingHour at zone =
    -- let
    --     a =
    --         Debug.log "At" at
    --     b =
    --         Debug.log "At Minute" (Time.toMinute zone at)
    -- in
    millisToPosix
        (posixToMillis at
            - Time.toMinute zone at
            * 60
            * 1000
            - Time.toSecond zone at
            * 1000
            - Time.toMillis zone at
        )



-- dayRingView : String -> Html Msg
-- dayRingView name =
--     div [ class "" ]
--         [ div [] [ text name ]
--         -- , List.range 0 24
--         ]
