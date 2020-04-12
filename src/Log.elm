module Log exposing
    ( Category(..)
    , Log
    , categoryToSlug
    , decoder
    , empty
    , encoder
    )

import Json.Decode as D
import Json.Encode as E


type alias Log =
    { title : String
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
    { title = ""
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
        [ ( "title", E.string log.title )
        , ( "category", categoryEncoder log.category )
        , ( "tags", E.list E.string log.tags )
        , ( "details", E.string log.details )
        ]


decoder : D.Decoder Log
decoder =
    D.map4 Log
        (D.field "title" D.string)
        (D.field "category" categoryDecoder)
        (D.field "tags" (D.list D.string))
        (D.field "details" D.string)


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
            ""


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
