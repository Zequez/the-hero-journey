module PosixExtra exposing
    ( add
    , diff
    , monthToInt
    , sort2
    , toNormalDate
    , toNormalDateTime
    , toNormalTime
    )

import String exposing (fromInt, padLeft)
import Time exposing (Month, Posix, Zone)


sort2 : ( Posix, Posix ) -> ( Posix, Posix )
sort2 ( start, end ) =
    if diff start end < 0 then
        ( end, start )

    else
        ( start, end )


add : Posix -> Int -> Posix
add posix millis =
    Time.millisToPosix (Time.posixToMillis posix + millis)


diff : Posix -> Posix -> Int
diff from to =
    Time.posixToMillis to - Time.posixToMillis from


monthToInt : Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            12


toNormalDateTime : Zone -> Posix -> String
toNormalDateTime tz posix =
    toNormalDate tz posix ++ " " ++ toNormalTime tz posix


toNormalDate : Zone -> Posix -> String
toNormalDate timeZone posix =
    [ Time.toYear timeZone posix
        |> fromInt
    , Time.toMonth timeZone posix
        |> monthToInt
        |> fromInt
        |> padLeft 2 '0'
    , Time.toDay timeZone posix
        |> fromInt
        |> padLeft 2 '0'
    ]
        |> String.join "-"


toNormalTime : Zone -> Posix -> String
toNormalTime timeZone posix =
    [ Time.toHour timeZone posix
        |> String.fromInt
        |> String.padLeft 2 '0'
    , Time.toMinute timeZone posix
        |> String.fromInt
        |> String.padLeft 2 '0'
    ]
        |> String.join ":"
