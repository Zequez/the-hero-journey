port module Ports exposing
    ( backupToLocalStorage
    , downloadBackup
    , receiveBackup
    , restoreFromLocalStorage
    , scrollViewportTo
    )


port backupToLocalStorage : String -> Cmd msg


port restoreFromLocalStorage : String -> Cmd msg


port downloadBackup : () -> Cmd msg


port receiveBackup : (String -> msg) -> Sub msg


port scrollViewportTo : Int -> Cmd msg
