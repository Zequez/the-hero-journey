module Timeline exposing
    ( Timeline
    , add
    , addAutoExpand
    , consolidateZeroLengthStops
    , decoder
    , empty
    , encoder
    , map
    , mapSplits
    , remove
    , resize
    , set
    , update
    )

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Log exposing (Log)
import PosixExtra as PXE
import Time exposing (Posix, posixToMillis)


type Timeline
    = Timeline (Array Log)



-- type Log
--     = Stop Posix
--     | Logd Posix data


empty : Timeline
empty =
    Timeline Array.empty


add : Log -> Posix -> (Log -> Bool) -> Int -> Timeline -> Timeline
add log end canOverrideCheck autoExpandMax (Timeline array) =
    if PXE.diff log.at end == 0 then
        addAutoExpand log autoExpandMax array
            |> Timeline

    else if PXE.diff log.at end < 0 then
        Timeline array

    else
        case makeSpaceFor log.at end canOverrideCheck array of
            Available newArray ->
                newArray
                    |> Array.push log
                    |> Array.push { log | at = end, visible = False }
                    |> reSort
                    |> Timeline

            EndstopExists newArray ->
                newArray
                    |> Array.push log
                    |> reSort
                    |> Timeline

            CouldntOverride ->
                array
                    |> Timeline


addAutoExpand : Log -> Int -> Array Log -> Array Log
addAutoExpand data maxDuration array =
    -- What a beautiful piece of machinery!
    case findBoundariesBy (posixToMillis data.at) (toPosix >> posixToMillis) array of
        OnTop _ ->
            array

        Unbounded ->
            array
                |> Array.push data
                |> Array.push { data | at = PXE.add data.at maxDuration, visible = False }
                |> reSort

        Dual top bottom ->
            case top.visible of
                False ->
                    if PXE.diff top.at bottom.at <= maxDuration then
                        array
                            |> Array.push { data | at = top.at }
                            |> reSort

                    else
                        array
                            |> Array.push { data | at = top.at }
                            |> Array.push { data | at = PXE.add top.at maxDuration, visible = False }
                            |> reSort

                True ->
                    array

        JustBottom bottom ->
            if PXE.diff data.at bottom.at < maxDuration then
                array
                    |> Array.push data
                    |> reSort

            else
                array
                    |> Array.push data
                    |> Array.push { data | at = PXE.add data.at maxDuration, visible = False }
                    |> reSort

        JustTop top ->
            if PXE.diff top.at data.at < maxDuration then
                array
                    |> Array.push { data | at = top.at }
                    |> Array.push { data | at = PXE.add top.at maxDuration, visible = False }
                    |> reSort

            else
                array
                    |> Array.push data
                    |> Array.push { data | at = PXE.add data.at maxDuration, visible = False }
                    |> reSort


set : Int -> Log -> Timeline -> Timeline
set i log (Timeline array) =
    case Array.get i array of
        Just _ ->
            array |> Array.set i log |> Timeline

        Nothing ->
            Timeline array


update : Int -> (Log -> Log) -> Timeline -> Timeline
update i updateFun (Timeline array) =
    case Array.get i array of
        Just split ->
            array |> Array.set i (updateFun split) |> Timeline

        Nothing ->
            Timeline array


remove : Int -> Timeline -> Timeline
remove i (Timeline array) =
    case Array.get i array of
        Just split ->
            array
                |> Array.set i { split | visible = False }
                |> consolidateEmpty
                |> Timeline

        _ ->
            Timeline array


resize : Int -> Posix -> Timeline -> Timeline
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
            if before <= current && current <= after then
                array
                    |> Array.set i (setSplitPosix posix split)
                    |> Timeline

            else if current == before then
                array
                    |> Array.set i (setSplitPosix posix split)
                    |> Timeline

            else if current == after then
                array
                    |> Array.set i (setSplitPosix posix split)
                    |> Timeline

            else
                Timeline array

        Nothing ->
            Timeline array


consolidateZeroLengthStops : Timeline -> Timeline
consolidateZeroLengthStops (Timeline array) =
    array
        |> Array.toIndexedList
        |> List.filterMap
            (\( i, split ) ->
                case split.visible of
                    False ->
                        case Array.get (i + 1) array of
                            Just nextSplit ->
                                if toMillis nextSplit == Time.posixToMillis split.at then
                                    Nothing

                                else
                                    Just split

                            _ ->
                                Just split

                    True ->
                        Just split
            )
        |> Array.fromList
        |> Timeline


map : (Int -> Maybe Log -> ( Posix, Posix ) -> a) -> Timeline -> List a
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


mapSplits : (( Int, Posix ) -> a) -> Timeline -> List a
mapSplits mapFun (Timeline array) =
    array
        |> Array.map toPosix
        |> Array.toIndexedList
        |> List.map mapFun


splitToMaybe : Log -> Maybe Log
splitToMaybe split =
    if split.visible == True then
        Just split

    else
        Nothing



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
    = Available (Array Log)
    | EndstopExists (Array Log)
    | CouldntOverride


makeSpaceFor : Posix -> Posix -> (Log -> Bool) -> Array Log -> MakeSpaceResult data
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


tryToDeleteEverythingBetween : Posix -> Posix -> (Log -> Bool) -> Array Log -> Array Log
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
                    case split.visible of
                        False ->
                            True

                        True ->
                            canOverrideCheck split

                else
                    False
            )


sliceBetween : Posix -> Posix -> Bool -> Array Log -> List ( Int, Log )
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


consolidateEmpty : Array Log -> Array Log
consolidateEmpty array =
    case Array.toList array of
        first :: tail ->
            tail
                |> List.map2
                    (\prevSplit split ->
                        case ( prevSplit.visible, split.visible ) of
                            ( False, False ) ->
                                Nothing

                            _ ->
                                Just split
                    )
                    (Array.toList array)
                |> List.filterMap identity
                |> (\list ->
                        case first.visible of
                            False ->
                                list

                            True ->
                                first :: list
                   )
                |> Array.fromList

        _ ->
            array


reSort : Array Log -> Array Log
reSort array =
    array
        |> Array.toList
        |> List.sortBy sortFun
        |> Array.fromList


setSplitPosix : Posix -> Log -> Log
setSplitPosix posix split =
    { split | at = posix }


getPosixAt : Int -> Array Log -> Maybe Posix
getPosixAt i array =
    array
        |> Array.get i
        |> Maybe.map toPosix


toPosix : Log -> Posix
toPosix split =
    split.at


toMillis : Log -> Int
toMillis =
    toPosix >> Time.posixToMillis


sortFun : Log -> Int
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


encoder : (Log -> E.Value) -> Timeline -> E.Value
encoder dataEncoder (Timeline array) =
    E.list dataEncoder (Array.toList array)



-- splitEncoder : (data -> E.Value) -> Log -> E.Value
-- splitEncoder dataEncoder split =
--     case split of
--         Stop posix ->
--             E.list identity [ posixEncoder posix, E.null ]
--         Logd posix data ->
--             E.list identity [ posixEncoder posix, dataEncoder data ]


decoder : D.Decoder Log -> D.Decoder Timeline
decoder dataDecoder =
    D.list dataDecoder
        |> D.map (\list -> Timeline (Array.fromList list))



-- splitDecoder : D.Decoder data -> D.Decoder (Log)
-- splitDecoder dataDecoder =
--     arrayAsTuple2 posixDecoder (D.nullable dataDecoder)
--         |> D.andThen
--             (\tuple ->
--                 case tuple of
--                     ( posix, Nothing ) ->
--                         D.succeed (Stop posix)
--                     ( posix, Just data ) ->
--                         D.succeed (Logd posix data)
--             )


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
