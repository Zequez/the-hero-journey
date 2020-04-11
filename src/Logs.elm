module Logs exposing
    ( Category(..)
    , Log
    , LogID
    , Logs
    , add
    , buildLog
    , categoryToSlug
    , consolidateEmpty
    , decoder
    , detectAllOverridable
    , encoder
    , findTopBottomMiddle
    , newID
    , nextID
    , remove
    , removeMany
    , sorted
    , update
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline as Dp
import Json.Encode as E
import PosixExtra as PXE
import Time exposing (Posix)



-- type alias Loggs = {
--   first: Log
--   , last: Log
--   , list: List Log
--   dict:
-- }


type alias Logs =
    Dict String Log


type alias Log =
    { id : LogID
    , title : String
    , category : Category
    , createdAt : Posix
    , at : Posix
    , tags : List String
    , details : String
    }


type alias LogID =
    String


type Category
    = SelfCare
    | Recreative
    | Creative
    | SelfGrowth
    | Uncategorized
    | Empty



-- Theoretically we initialize Logs with at least 2 logs, so
-- in any case we evaluate the list of logs to be less than 2
-- we return this to satisfy the compiler.
-- However, we should think about changing the datastructure.


impossibleStartLog : Log
impossibleStartLog =
    { id = "-1"
    , title = ""
    , category = Empty
    , createdAt = Time.millisToPosix 0
    , at = Time.millisToPosix 0
    , tags = []
    , details = ""
    }


impossibleEndLog : Log
impossibleEndLog =
    { impossibleStartLog
        | id = "999999999"
        , createdAt = Time.millisToPosix (1000 * 60 * 60 * 24 * 365 * 1000)
        , at = Time.millisToPosix (1000 * 60 * 60 * 24 * 365 * 1000)
    }


newID : Logs -> LogID
newID logs =
    String.fromInt
        (1
            + (Dict.values logs
                |> List.map .id
                |> List.map (\str -> Maybe.withDefault 0 (String.toInt str))
                |> List.maximum
                |> Maybe.withDefault 0
              )
        )


nextID : LogID -> LogID
nextID id =
    String.fromInt (Maybe.withDefault 0 (String.toInt id) + 1)


update : LogID -> (Log -> Log) -> Logs -> Logs
update logId updateFun =
    Dict.update logId (Maybe.map updateFun)


add : Log -> Logs -> Logs
add log logs =
    Dict.insert log.id log logs


remove : Log -> Logs -> Logs
remove log logs =
    Dict.remove log.id logs


removeMany : List Log -> Logs -> Logs
removeMany logsList logs =
    logsList
        |> List.foldl remove logs


buildLog : LogID -> Posix -> Posix -> Log
buildLog logID createdAt posixAt =
    -- in
    { id = logID
    , title = ""
    , category = Uncategorized
    , createdAt = createdAt
    , at = posixAt
    , tags = []
    , details = ""
    }


consolidateEmpty : Logs -> Logs
consolidateEmpty logs =
    let
        allLogs =
            sorted logs
    in
    case allLogs of
        _ :: rest ->
            rest
                |> List.map2 detectUnnecesaryEmpty allLogs
                |> List.filterMap identity
                |> List.foldl (\log newLogs -> Dict.remove log.id newLogs) logs

        [] ->
            logs


detectUnnecesaryEmpty : Log -> Log -> Maybe Log
detectUnnecesaryEmpty log1 log2 =
    if log1.category == Empty && log2.category == Empty then
        Just log2

    else
        Nothing


detectAllOverridable : List Log -> Bool
detectAllOverridable logsList =
    logsList
        |> List.all (\log -> log.category == Empty || (log.category == Uncategorized && log.title == ""))


sorted : Logs -> List Log
sorted logs =
    Dict.values logs |> List.sortBy sortFun


sortFun : Log -> Int
sortFun log =
    Time.posixToMillis log.at



-- findTop : Logs -> Posix -> Log
-- findTop logs posix =
--     let
--         logsList =
--             sorted logs
--     in
--       case logsList of
--         _ :: rest ->
--           rest
--             |> List.foldl (\log -> )
--         [] -> impossibleFirstLog


findTopBottomMiddle : Logs -> Posix -> Posix -> ( Log, List Log, Log )
findTopBottomMiddle logs from to =
    let
        sortedList =
            sorted logs

        ( top, rest ) =
            partition sortedList from

        ( middle, bottom ) =
            partition rest to

        -- _ =
        --     [ Debug.log "Top: " top
        --     , Debug.log "Middle: " middle
        --     , Debug.log "Bottom: " bottom
        --     ]
    in
    ( Maybe.withDefault impossibleStartLog (List.head (List.reverse top))
    , middle
    , Maybe.withDefault impossibleEndLog (List.head bottom)
    )


partition : List Log -> Posix -> ( List Log, List Log )
partition logs at =
    logs
        |> List.partition (\log -> PXE.diff log.at at > 0)



-- findInBetween : Logs -> Posix -> Posix -> List Log
-- findInBetween logs from to =
--     let
--         logsList =
--             sorted logs
--     in
--     []
-- ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
-- ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
-- █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
-- ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
-- ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
-- ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


encoder : Logs -> E.Value
encoder =
    E.dict identity logEncode


decoder : D.Decoder Logs
decoder =
    D.dict logDecode


logEncode : Log -> E.Value
logEncode log =
    E.object
        [ ( "id", E.string log.id )
        , ( "title", E.string log.title )
        , ( "category", categoryEncoder log.category )
        , ( "createdAt", posixEncoder log.createdAt )
        , ( "at", posixEncoder log.at )
        , ( "tags", E.list E.string log.tags )
        , ( "details", E.string log.details )
        ]


logDecode : D.Decoder Log
logDecode =
    D.succeed Log
        |> Dp.required "id" D.string
        |> Dp.required "title" D.string
        |> Dp.required "category" categoryDecoder
        |> Dp.required "createdAt" posixDecoder
        |> Dp.required "at" posixDecoder
        |> Dp.required "tags" (D.list D.string)
        |> Dp.required "details" D.string


posixEncoder : Posix -> E.Value
posixEncoder time =
    E.int (Time.posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))


categoryEncoder : Category -> E.Value
categoryEncoder category =
    E.string (categoryToSlug category)


categoryToSlug : Category -> String
categoryToSlug category =
    case category of
        SelfCare ->
            "SelfCare"

        Recreative ->
            "Recreative"

        Creative ->
            "Creative"

        SelfGrowth ->
            "SelfGrowth"

        Uncategorized ->
            "Uncategorized"

        Empty ->
            "Empty"


categoryDecoder : D.Decoder Category
categoryDecoder =
    D.string
        |> D.andThen
            (\str ->
                D.succeed
                    (case str of
                        "SelfCare" ->
                            SelfCare

                        "Recreative" ->
                            Recreative

                        "Creative" ->
                            Creative

                        "SelfGrowth" ->
                            SelfGrowth

                        "Uncategorized" ->
                            Uncategorized

                        _ ->
                            Empty
                    )
            )
