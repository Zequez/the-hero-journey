module Viewport exposing
    ( Viewport
    , day
    , endDate
    , fullHeight
    , hour
    , millisToPx
    , posixAddPx
    , posixToPx
    , pxPerDay
    , pxToMillis
    , pxToPosix
    , startDate
    )

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
