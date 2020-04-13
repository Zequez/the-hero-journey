module TimeLayer exposing (view)

import Html as H exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import PosixExtra as PXE
import Time exposing (Posix)
import TimePx exposing (Ratio, Viewport)


view : Viewport -> Time.Zone -> Time.Posix -> Html msg
view vp zone time =
    div [ class "viewport-time-layer" ]
        [ div
            (class "viewport-time-marks"
                :: repeatingGradient vp zone
            )
            []
        , div
            [ class "viewport-time-now"
            , style "top" (PXE.diff vp.from time |> TimePx.intToPx vp.ratio)
            ]
            []
        ]


repeatingGradient : Viewport -> Time.Zone -> List (Attribute msg)
repeatingGradient vp zone =
    let
        shift =
            -(Time.toMinute zone vp.from
                * (60 * 1000)
                + (Time.toSecond zone vp.from * 1000)
                + Time.toMillis zone vp.from
                |> modBy (60 * 60 * 1000)
             )
                |> TimePx.intToPx vp.ratio

        size =
            (60 * 60 * 1000) |> TimePx.intToPx vp.ratio

        _ =
            Debug.log "Timelayer render"
                { shift = shift
                , hourlyPx = size
                }
    in
    [ style "top" shift
    , style
        "background-image"
        ("repeating-linear-gradient(180deg, rgba(0,0,0,0.1) 0 1px, transparent 0 " ++ size ++ ")")
    ]
