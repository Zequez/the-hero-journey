module Logs exposing
    ( Log
    , Logs
    , all
    , last
    , sort
    )

import Time exposing (Posix)


type LogID
    = String


type alias Log =
    { id : LogID
    , title : String
    , category : Category
    , createdAt : Posix
    , at : Posix
    , tags : List String
    , details : String
    }


type alias Logs =
    { first : Log
    , second : Log
    , all : Dict Log
    }


type Category
    = SelfCare
    | Recreative
    | Creative
    | SelfGrowth
    | Uncategorized
    | Empty


all : Logs -> Array Log
all logs =
    logs.first :: logs.second :: logs.rest


last : Logs -> Log
last logs =
    if List.isEmpty logs.rest then
        logs.second

    else
        case List.reverse logs.rest of
            lastItem :: _ ->
                lastItem

            -- Should never happen
            _ ->
                logs.second


add : Log -> Logs -> Logs
add log logs =
    sort { logs | rest = log :: logs.rest }



-- remove : Log -> Logs -> Logs
-- remove log logs =
--   case logs.rest of
--     [] -> logs
--     third :: rest ->


sort : Logs -> Logs
sort logs =
    case all logs |> List.sortBy sortFun of
        first :: second :: rest ->
            Logs first second rest

        -- Should never happen
        _ ->
            logs


sortFun : Log -> Int
sortFun log =
    Time.posixToMillis log.at
