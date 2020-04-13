module Viewport exposing
    ( Viewport
    , day
    , endDate
    , fullHeight
    , hour
    , millisToPx
    , millisToPxFloat
    , posixAddPx
    , posixToPx
    , posixToPxFloat
    , pxPerDay
    , pxToMillis
    , pxToPosix
    , repeatingGradient
    , startDate
    )

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import PosixExtra
import Time exposing (Posix)


type alias Viewport =
    { firstDate : Posix
    , lastDate : Posix
    , visibleTimespan : Int -- on millis
    , scroll : Int
    , height : Int
    }


type alias RatioData a =
    { a | height : Int, visibleTimespan : Int }


repeatingGradient : Viewport -> Time.Zone -> List (Attribute msg)
repeatingGradient vp zone =
    let
        -- shift =
        --     (((Time.toMinute zone vp.firstDate * (60 * 1000))
        --         + (Time.toSecond zone vp.firstDate * 1000)
        --         + Time.toMillis zone vp.firstDate
        --      )
        --         |> modBy (60 * 60 * 1000)
        --     )
        --         |> millisToPxFloat vp
        size =
            (60 * 60 * 1000)
                |> millisToPxFloat vp
    in
    [ style
        "background-image"
        ("repeating-linear-gradient(180deg, rgba(0,0,0,0.1) 0 1px, transparent 0 " ++ size ++ ")")

    -- , style "top" shift
    ]


startDate : Viewport -> Posix
startDate viewport =
    viewport.firstDate
        |> posixAddPx viewport viewport.scroll


endDate : Viewport -> Posix
endDate viewport =
    viewport.firstDate
        |> posixAddPx viewport (viewport.scroll + viewport.height)


fullHeight : Viewport -> Int
fullHeight viewport =
    PosixExtra.diff viewport.firstDate viewport.lastDate
        |> millisToPx viewport


millisToPx : RatioData a -> Int -> Int
millisToPx viewport millis =
    (millis * pxPerDay viewport) // day


millisToPxFloat : RatioData a -> Int -> String
millisToPxFloat viewport millis =
    String.fromFloat (toFloat (millis * pxPerDay viewport) / toFloat day) ++ "px"


pxToMillis : RatioData a -> Int -> Int
pxToMillis viewport pixels =
    (pixels * day) // pxPerDay viewport


pxToPosix : RatioData a -> Int -> Posix
pxToPosix viewport pixels =
    pxToMillis viewport pixels
        |> Time.millisToPosix


posixAddPx : RatioData a -> Int -> Posix -> Posix
posixAddPx viewport pixels posix =
    PosixExtra.add posix (pxToMillis viewport pixels)


posixToPxFloat : RatioData a -> Posix -> String
posixToPxFloat viewport posix =
    Time.posixToMillis posix
        |> millisToPxFloat viewport


posixToPx : RatioData a -> Posix -> Int
posixToPx viewport posix =
    Time.posixToMillis posix
        |> millisToPx viewport


pxPerDay : RatioData a -> Int
pxPerDay vp =
    vp.height * day // vp.visibleTimespan


day : Int
day =
    hour * 24


hour : Int
hour =
    60 * 60 * 1000
