module Log exposing
    ( Category(..)
    , Log
    , categoryToSlug
    , decoder
    , empty
    , encoder
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Time exposing (Posix)


type alias Log =
    { id : Maybe String
    , at : Posix
    , visible : Bool
    , title : String
    , category : Maybe Category
    , tags : List String
    , details : String
    }


type Category
    = SelfCare
    | Recreative
    | Creative
    | SelfGrowth


empty : Log
empty =
    { id = Nothing
    , at = Time.millisToPosix 0
    , visible = True
    , title = ""
    , category = Nothing
    , tags = []
    , details = ""
    }



-- ███████╗███╗   ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗ ███████╗
-- ██╔════╝████╗  ██║██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
-- █████╗  ██╔██╗ ██║██║     ██║   ██║██║  ██║█████╗  ██████╔╝███████╗
-- ██╔══╝  ██║╚██╗██║██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗╚════██║
-- ███████╗██║ ╚████║╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║███████║
-- ╚══════╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝


encoder : Log -> E.Value
encoder log =
    E.object
        [ ( "id", maybeStringEncoder log.id )
        , ( "at", posixEncoder log.at )
        , ( "visible", E.bool log.visible )
        , ( "title", E.string log.title )
        , ( "category", categoryEncoder log.category )
        , ( "tags", E.list E.string log.tags )
        , ( "details", E.string log.details )
        ]


maybeStringEncoder : Maybe String -> E.Value
maybeStringEncoder val =
    Maybe.withDefault E.null (Maybe.map (\i -> E.string i) val)


decoder : D.Decoder Log
decoder =
    D.succeed Log
        |> DP.optional "id" (D.nullable D.string) Nothing
        |> DP.optional "at" posixDecoder (Time.millisToPosix 0)
        |> DP.optional "visible" D.bool True
        |> DP.required "title" D.string
        |> DP.required "category" categoryDecoder
        |> DP.required "tags" (D.list D.string)
        |> DP.required "details" D.string


categoryEncoder : Maybe Category -> E.Value
categoryEncoder category =
    E.string (categoryToSlug category)


categoryToSlug : Maybe Category -> String
categoryToSlug maybeCategory =
    case maybeCategory of
        Just category ->
            case category of
                SelfCare ->
                    "SelfCare"

                Recreative ->
                    "Recreative"

                Creative ->
                    "Creative"

                SelfGrowth ->
                    "SelfGrowth"

        Nothing ->
            "Uncategorized"


categoryDecoder : D.Decoder (Maybe Category)
categoryDecoder =
    D.nullable D.string
        |> D.andThen
            (\str ->
                D.succeed
                    (case str of
                        Just "SelfCare" ->
                            Just SelfCare

                        Just "Recreative" ->
                            Just Recreative

                        Just "Creative" ->
                            Just Creative

                        Just "SelfGrowth" ->
                            Just SelfGrowth

                        _ ->
                            Nothing
                    )
            )


posixEncoder : Posix -> E.Value
posixEncoder time =
    E.int (Time.posixToMillis time)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\millis -> D.succeed (Time.millisToPosix millis))
