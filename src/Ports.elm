port module Ports exposing
    ( backupToLocalStorage
    , receiveBackup
    , restoreFromLocalStorage
    )


port backupToLocalStorage : String -> Cmd msg


port restoreFromLocalStorage : String -> Cmd msg


port receiveBackup : (String -> msg) -> Sub msg
