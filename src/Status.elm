module Status
    ( Status
    , Action(Overlay, NoOverlay, Main), update
    , message
    , view
    ) where

import Html exposing (Html, div, text)


type alias Status = { overlay : Maybe String, main : String }

type Action = Overlay String | NoOverlay | Main String

-- Update the status

update : Action -> Status -> Status
update action status =
    case action of
        Overlay msg ->
            { status | overlay = Just msg }
        NoOverlay ->
            { status | overlay = Nothing }
        Main msg ->
            { status | main = msg }

-- Get the right message from the status

message : Status -> String
message status =
    Maybe.withDefault status.main status.overlay

-- Render the status bar

view : Status -> Html
view status =
    div [] [ message status |> text ]

