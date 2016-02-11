module Status (Status, message) where

type alias Status = { overlay : Maybe String, main : String }

message : Status -> String
message status =
    Maybe.withDefault status.main status.overlay

