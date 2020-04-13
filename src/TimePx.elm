module TimePx exposing
    ( Ratio
    , Viewport
    , deltaIntToNum
    , deltaIntToPx
    , deltaPosixToNum
    , deltaPosixToPx
    , intToPx
    , pxToPosixPosition
    , ratioFromScreen
    , snapBy
    , viewportDeltaPosixToPx
    , viewportHeightToNum
    , viewportHeightToPx
    )

import Time exposing (Posix)


type alias Viewport =
    { from : Posix
    , to : Posix
    , ratio : Float
    }


type alias Ratio =
    Float


pxToPosixPosition : Viewport -> Int -> Posix
pxToPosixPosition vp pixelPosition =
    let
        millis =
            toFloat pixelPosition / vp.ratio

        viewportFrom =
            posixToFloat vp.from
    in
    Time.millisToPosix (round (viewportFrom + millis))


snapBy : Int -> Posix -> Posix
snapBy snap posix =
    let
        num =
            Time.posixToMillis posix

        snapDistance =
            modBy snap num
    in
    Time.millisToPosix <|
        if snapDistance < (snap // 2) then
            num - snapDistance

        else
            num + (snap - snapDistance)


ratioFromScreen : Int -> Int -> Ratio
ratioFromScreen visibleViewportHeight hoursPerHeight =
    toFloat visibleViewportHeight / toFloat hoursPerHeight



------------ VIEWPORT TO PX


viewportHeightToNum : Viewport -> Float
viewportHeightToNum vp =
    deltaPosixToNum vp.ratio vp.from vp.to


viewportHeightToPx : Viewport -> String
viewportHeightToPx vp =
    deltaPosixToPx vp.ratio vp.from vp.to


viewportDeltaPosixToPx : Viewport -> Posix -> String
viewportDeltaPosixToPx vp to =
    deltaPosixToPx vp.ratio vp.from to



---------- POSIX TO PX


deltaPosixToNum : Ratio -> Posix -> Posix -> Float
deltaPosixToNum ratio from to =
    (posixToFloat to - posixToFloat from) * ratio


deltaPosixToPx : Ratio -> Posix -> Posix -> String
deltaPosixToPx ratio from to =
    deltaPosixToNum ratio from to |> px



--- INT TO PX


deltaIntToNum : Ratio -> Int -> Int -> Float
deltaIntToNum ratio from to =
    (toFloat to - toFloat from) * ratio


deltaIntToPx : Ratio -> Int -> Int -> String
deltaIntToPx ratio from to =
    deltaIntToNum ratio from to |> px


intToPx : Ratio -> Int -> String
intToPx ratio num =
    deltaIntToPx ratio 0 num



------------------- OTHER


posixToFloat : Posix -> Float
posixToFloat =
    Time.posixToMillis >> toFloat


px : Float -> String
px num =
    String.fromFloat num ++ "px"
