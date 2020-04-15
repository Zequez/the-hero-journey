port module Backend exposing
    ( ErrorData
    , UserData
    , logInErrorDecoder
    , logsListDecoder
    , receiveLogs
    , saveLog
    , signIn
    , signInError
    , signInInfo
    , signOut
    , userDataDecoder
    )

import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Log exposing (Log)


type alias ErrorData =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


logsListDecoder : D.Decoder (List Log)
logsListDecoder =
    D.map identity
        (D.field "logs" (D.list Log.decoder))


userDataDecoder : D.Decoder UserData
userDataDecoder =
    D.succeed UserData
        |> DP.required "token" D.string
        |> DP.required "email" D.string
        |> DP.required "uid" D.string


logInErrorDecoder : D.Decoder ErrorData
logInErrorDecoder =
    D.succeed ErrorData
        |> DP.required "code" (D.nullable D.string)
        |> DP.required "message" (D.nullable D.string)
        |> DP.required "credential" (D.nullable D.string)


port signIn : () -> Cmd msg


port signInInfo : (E.Value -> msg) -> Sub msg


port signInError : (E.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port saveLog : E.Value -> Cmd msg


port receiveLogs : (E.Value -> msg) -> Sub msg
