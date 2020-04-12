module Timeline exposing
    ( add
    , decoder
    , empty
    , encoder
    , remove
    , resize
    , set
    )

import Array exposing (Array)
import Array.Extra
import Json.Decode as D
import Json.Encode as E
import Time exposing (Posix)


type Timeline data
    = Timeline (Array (Split data))


type Split data
    = Stop Posix
    | Logd Posix data


empty : Timeline data
empty =
    Timeline Array.empty


add : data -> Posix -> Posix -> (data -> Bool) -> Timeline data -> Timeline data
add data start end dataIsUseless (Timeline array) =
    case makeSpaceFor start end dataIsUseless array of
        Available newArray ->
            newArray
                |> Array.push (Logd start data)
                |> Array.push (Stop end)
                |> reSort
                |> Timeline

        MovedOne newArray ->
            newArray
                |> Array.push (Logd start data)
                |> reSort
                |> Timeline

        CouldntOverride ->
            array
                |> Timeline


type MakeSpaceResult data
    = Available (Array (Split data))
    | MovedOne (Array (Split data))
    | CouldntOverride


makeSpaceFor : Posix -> Posix -> (data -> Bool) -> Array (Split data) -> MakeSpaceResult data
makeSpaceFor start end dataIsUseless array =
    let
        preCleanArray =
            array
                |> Array.Extra.removeWhen
                    (isBetweenAndSafeToOverride start end dataIsUseless)

        leftBetween =
            preCleanArray
                |> sliceBetween start end
    in
    case leftBetween of
        [] ->
            preCleanArray
                |> Available

        ( i, justTheOneSplit ) :: [] ->
            preCleanArray
                |> Array.set i (setSplitPosix end justTheOneSplit)
                |> MovedOne

        _ ->
            CouldntOverride


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


remove : Int -> Timeline data -> Timeline data
remove i (Timeline array) =
    array
        |> Array.Extra.removeAt i
        |> Timeline


resize : Int -> Posix -> Timeline data -> Timeline data
resize i posix (Timeline array) =
    let
        before =
            array
                |> getPosixAt (i - 1)
                |> Maybe.map Time.posixToMillis
                |> Maybe.map toFloat
                |> Maybe.withDefault -(1 / 0)

        after =
            array
                |> getPosixAt (i + 1)
                |> Maybe.map Time.posixToMillis
                |> Maybe.map toFloat
                |> Maybe.withDefault (1 / 0)

        current =
            posix
                |> Time.posixToMillis
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



-- ██╗███╗   ██╗████████╗███████╗██████╗ ███╗   ██╗ █████╗ ██╗     ███████╗
-- ██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗████╗  ██║██╔══██╗██║     ██╔════╝
-- ██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██╔██╗ ██║███████║██║     ███████╗
-- ██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██║╚██╗██║██╔══██║██║     ╚════██║
-- ██║██║ ╚████║   ██║   ███████╗██║  ██║██║ ╚████║██║  ██║███████╗███████║
-- ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝╚══════╝


setSplitPosix : Posix -> Split data -> Split data
setSplitPosix posix split =
    case split of
        Stop _ ->
            Stop posix

        Logd _ log ->
            Logd posix log


isBetweenAndSafeToOverride : Posix -> Posix -> (data -> Bool) -> Split data -> Bool
isBetweenAndSafeToOverride start end dataIsUseless split =
    splitIsBetween (Time.posixToMillis start) (Time.posixToMillis end) split
        && isSafeToOverride dataIsUseless split


isSafeToOverride : (data -> Bool) -> Split data -> Bool
isSafeToOverride dataIsUseless split =
    case split of
        Stop _ ->
            True

        Logd _ log ->
            dataIsUseless log


sliceBetween : Posix -> Posix -> Array (Split data) -> List ( Int, Split data )
sliceBetween from to arr =
    arr
        |> Array.toIndexedList
        |> List.filter
            (\( _, split ) ->
                splitIsBetween
                    (Time.posixToMillis from)
                    (Time.posixToMillis to)
                    split
            )


splitIsBetween : Int -> Int -> Split data -> Bool
splitIsBetween from to split =
    let
        posix =
            split
                |> unwrapPosix
                |> Time.posixToMillis
    in
    from < posix && posix < to


reSort : Array (Split data) -> Array (Split data)
reSort array =
    array
        |> Array.toList
        |> List.sortBy sortFun
        |> Array.fromList


getPosixAt : Int -> Array (Split data) -> Maybe Posix
getPosixAt i array =
    array
        |> Array.get i
        |> Maybe.map unwrapPosix


unwrapPosix : Split data -> Posix
unwrapPosix split =
    case split of
        Stop posix ->
            posix

        Logd posix _ ->
            posix


sortFun : Split data -> Int
sortFun split =
    split
        |> unwrapPosix
        |> Time.posixToMillis



-- ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
-- ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
-- █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
-- ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
-- ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
-- ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


encoder : Timeline data -> (data -> E.Value) -> E.Value
encoder (Timeline array) dataEncoder =
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
    E.int (Time.posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))
