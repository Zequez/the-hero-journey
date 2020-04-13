module Timeline exposing
    ( Split(..)
    , Timeline
    , add
    , addAutoExpand
    , decoder
    , empty
    , encoder
    , map
    , remove
    , resize
    , set
    , update
    )

import Array exposing (Array)
import Array.Extra
import Json.Decode as D
import Json.Encode as E
import PosixExtra as PXE
import Time exposing (Posix, posixToMillis)


type Timeline data
    = Timeline (Array (Split data))


type Split data
    = Stop Posix
    | Logd Posix data


empty : Timeline data
empty =
    Timeline Array.empty


add : data -> Posix -> Posix -> (data -> Bool) -> Int -> Timeline data -> Timeline data
add data unsortedStart unsortedEnd canOverrideCheck autoExpandMax (Timeline array) =
    if PXE.diff unsortedStart unsortedEnd == 0 then
        addAutoExpand data unsortedStart autoExpandMax array
            |> Timeline

    else
        let
            ( start, end ) =
                PXE.sort2 ( unsortedStart, unsortedEnd )
        in
        case makeSpaceFor start end canOverrideCheck array of
            Available newArray ->
                newArray
                    |> Array.push (Logd start data)
                    |> Array.push (Stop end)
                    |> reSort
                    |> Timeline

            EndstopExists newArray ->
                newArray
                    |> Array.push (Logd start data)
                    |> reSort
                    |> Timeline

            CouldntOverride ->
                array
                    |> Timeline


addAutoExpand : data -> Posix -> Int -> Array (Split data) -> Array (Split data)
addAutoExpand data point maxDuration array =
    -- What a beautiful piece of machinery!
    case findBoundariesBy (posixToMillis point) (toPosix >> posixToMillis) array of
        OnTop _ ->
            array

        Unbounded ->
            array
                |> Array.push (Logd point data)
                |> Array.push (Stop (PXE.add point maxDuration))
                |> reSort

        Dual top bottom ->
            case top of
                Stop topPosix ->
                    if PXE.diff topPosix (toPosix bottom) <= maxDuration then
                        array
                            |> Array.push (Logd topPosix data)
                            |> reSort

                    else
                        array
                            |> Array.push (Logd topPosix data)
                            |> Array.push (Stop (PXE.add topPosix maxDuration))
                            |> reSort

                Logd _ _ ->
                    array

        JustBottom bottom ->
            if PXE.diff point (toPosix bottom) < maxDuration then
                array
                    |> Array.push (Logd point data)
                    |> reSort

            else
                array
                    |> Array.push (Logd point data)
                    |> Array.push (Stop (PXE.add point maxDuration))
                    |> reSort

        JustTop _ ->
            array
                |> Array.push (Logd point data)
                |> Array.push (Stop (PXE.add point maxDuration))
                |> reSort


set : Int -> data -> Timeline data -> Timeline data
set i log (Timeline array) =
    let
        maybePosix =
            getPosixAt i array
    in
    case maybePosix of
        Just posix ->
            array
                |> Array.set i (Logd posix log)
                |> Timeline

        Nothing ->
            Timeline array


update : Int -> (data -> data) -> Timeline data -> Timeline data
update i updateFun (Timeline array) =
    let
        maybeSplit =
            array
                |> Array.get i
    in
    case maybeSplit of
        Just split ->
            case split of
                Logd posix data ->
                    array
                        |> Array.set i (Logd posix (updateFun data))
                        |> Timeline

                Stop _ ->
                    Timeline array

        Nothing ->
            Timeline array


remove : Int -> Timeline data -> Timeline data
remove i (Timeline array) =
    case Array.get i array of
        Just (Logd posix _) ->
            array
                |> Array.set i (Stop posix)
                |> consolidateEmpty
                |> Timeline

        _ ->
            Timeline array


resize : Int -> Posix -> Timeline data -> Timeline data
resize i posix (Timeline array) =
    let
        before =
            array
                |> getPosixAt (i - 1)
                |> Maybe.map posixToMillis
                |> Maybe.map toFloat
                |> Maybe.withDefault -(1 / 0)

        after =
            array
                |> getPosixAt (i + 1)
                |> Maybe.map posixToMillis
                |> Maybe.map toFloat
                |> Maybe.withDefault (1 / 0)

        current =
            posix
                |> posixToMillis
                |> toFloat

        maybeSplit =
            array
                |> Array.get i
    in
    case maybeSplit of
        Just split ->
            if before > current && current < after then
                array
                    |> Array.set i (setSplitPosix posix split)
                    |> Timeline

            else
                Timeline array

        Nothing ->
            Timeline array


map : (Int -> Maybe data -> ( Posix, Posix ) -> a) -> Timeline data -> List a
map mapFun (Timeline array) =
    let
        shifted =
            array
                |> Array.slice 1 (Array.length array)
                |> Array.toList
    in
    array
        |> Array.toIndexedList
        |> List.map2
            (\next ( index, first ) ->
                mapFun index (splitToMaybe first) ( toPosix first, toPosix next )
            )
            shifted


splitToMaybe : Split data -> Maybe data
splitToMaybe split =
    case split of
        Stop _ ->
            Nothing

        Logd _ data ->
            Just data



-- ██╗███╗   ██╗████████╗███████╗██████╗ ███╗   ██╗ █████╗ ██╗     ███████╗
-- ██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗████╗  ██║██╔══██╗██║     ██╔════╝
-- ██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██╔██╗ ██║███████║██║     ███████╗
-- ██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██║╚██╗██║██╔══██║██║     ╚════██║
-- ██║██║ ╚████║   ██║   ███████╗██║  ██║██║ ╚████║██║  ██║███████╗███████║
-- ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝╚══════╝


type FindBoundariesResult a
    = Unbounded
    | OnTop a
    | JustTop a
    | JustBottom a
    | Dual a a


findBoundariesBy : comparable -> (a -> comparable) -> Array a -> FindBoundariesResult a
findBoundariesBy query mapFun list =
    let
        ( maybeTop, maybeBottom ) =
            list
                |> Array.toList
                |> List.map (\a -> ( a, mapFun a ))
                |> List.partition (\( _, comparable ) -> comparable <= query)
                |> Tuple.mapFirst (List.reverse >> List.head >> Maybe.map Tuple.first)
                |> Tuple.mapSecond (List.head >> Maybe.map Tuple.first)
    in
    case ( maybeTop, maybeBottom ) of
        ( Just top, _ ) ->
            if mapFun top == query then
                OnTop top

            else
                case maybeBottom of
                    Just bottom ->
                        Dual top bottom

                    Nothing ->
                        JustTop top

        ( Nothing, Just bottom ) ->
            JustBottom bottom

        ( Nothing, Nothing ) ->
            Unbounded


type MakeSpaceResult data
    = Available (Array (Split data))
    | EndstopExists (Array (Split data))
    | CouldntOverride


makeSpaceFor : Posix -> Posix -> (data -> Bool) -> Array (Split data) -> MakeSpaceResult data
makeSpaceFor start end canOverrideCheck array =
    let
        slice =
            array
                |> sliceBetween start end True
                |> List.reverse
    in
    case slice of
        [] ->
            array |> Available

        ( i, lastItem ) :: _ ->
            let
                result =
                    array
                        |> Array.set i (setSplitPosix end lastItem)
                        |> tryToDeleteEverythingBetween start end canOverrideCheck
            in
            if result |> sliceBetween start end False |> List.isEmpty then
                EndstopExists result

            else
                CouldntOverride


tryToDeleteEverythingBetween : Posix -> Posix -> (data -> Bool) -> Array (Split data) -> Array (Split data)
tryToDeleteEverythingBetween start end canOverrideCheck array =
    let
        top =
            posixToMillis start

        bottom =
            posixToMillis end
    in
    array
        |> Array.Extra.removeWhen
            (\split ->
                if toMillis split >= top && toMillis split < bottom then
                    case split of
                        Stop _ ->
                            True

                        Logd _ log ->
                            canOverrideCheck log

                else
                    False
            )


sliceBetween : Posix -> Posix -> Bool -> Array (Split data) -> List ( Int, Split data )
sliceBetween start end includeLast arr =
    let
        top =
            posixToMillis start

        bottom =
            posixToMillis end
    in
    arr
        |> Array.toIndexedList
        |> List.filter
            (\( _, split ) ->
                -- Intentionally include the last one
                if includeLast then
                    toMillis split >= top && toMillis split <= bottom

                else
                    toMillis split >= top && toMillis split < bottom
            )


consolidateEmpty : Array (Split data) -> Array (Split data)
consolidateEmpty array =
    case Array.toList array of
        first :: tail ->
            tail
                |> List.map2
                    (\prevSplit split ->
                        case ( prevSplit, split ) of
                            ( Stop _, Stop _ ) ->
                                Nothing

                            _ ->
                                Just split
                    )
                    (Array.toList array)
                |> List.filterMap identity
                |> (\list ->
                        case first of
                            Stop _ ->
                                list

                            _ ->
                                first :: list
                   )
                |> Array.fromList

        _ ->
            array


reSort : Array (Split data) -> Array (Split data)
reSort array =
    array
        |> Array.toList
        |> List.sortBy sortFun
        |> Array.fromList


setSplitPosix : Posix -> Split data -> Split data
setSplitPosix posix split =
    case split of
        Stop _ ->
            Stop posix

        Logd _ log ->
            Logd posix log


getPosixAt : Int -> Array (Split data) -> Maybe Posix
getPosixAt i array =
    array
        |> Array.get i
        |> Maybe.map toPosix


toPosix : Split data -> Posix
toPosix split =
    case split of
        Stop posix ->
            posix

        Logd posix _ ->
            posix


toMillis : Split data -> Int
toMillis =
    toPosix >> Time.posixToMillis


sortFun : Split data -> Int
sortFun split =
    split
        |> toPosix
        |> posixToMillis



-- ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
-- ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
-- █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
-- ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
-- ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
-- ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


encoder : (data -> E.Value) -> Timeline data -> E.Value
encoder dataEncoder (Timeline array) =
    E.list (splitEncoder dataEncoder) (Array.toList array)


splitEncoder : (data -> E.Value) -> Split data -> E.Value
splitEncoder dataEncoder split =
    case split of
        Stop posix ->
            E.list identity [ posixEncoder posix, E.null ]

        Logd posix data ->
            E.list identity [ posixEncoder posix, dataEncoder data ]


decoder : D.Decoder data -> D.Decoder (Timeline data)
decoder dataDecoder =
    D.list (splitDecoder dataDecoder)
        |> D.map (\list -> Timeline (Array.fromList list))


splitDecoder : D.Decoder data -> D.Decoder (Split data)
splitDecoder dataDecoder =
    arrayAsTuple2 posixDecoder (D.nullable dataDecoder)
        |> D.andThen
            (\tuple ->
                case tuple of
                    ( posix, Nothing ) ->
                        D.succeed (Stop posix)

                    ( posix, Just data ) ->
                        D.succeed (Logd posix data)
            )


arrayAsTuple2 : D.Decoder a -> D.Decoder b -> D.Decoder ( a, b )
arrayAsTuple2 a b =
    D.index 0 a
        |> D.andThen
            (\aVal ->
                D.index 1 b
                    |> D.andThen (\bVal -> D.succeed ( aVal, bVal ))
            )


posixEncoder : Posix -> E.Value
posixEncoder time =
    E.int (posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))
