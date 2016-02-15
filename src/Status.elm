module Status
    ( Status
    , Action(Rollover, NoRollover, Main, NoMain), update
    , view
    ) where

import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Maybe exposing (withDefault)


type alias Status = { rollover : Maybe String, main : Maybe String }

type Action
    = Rollover String
    | NoRollover
    | Main String
    | NoMain

-- Update the status

update : Action -> Status -> Status
update action status =
    case action of
        Rollover msg ->
            { status | rollover = Just msg }
        NoRollover ->
            { status | rollover = Nothing }
        Main msg ->
            { status | main = Just msg }
        NoMain ->
            { status | main = Nothing }

-- Render the status bar

view : Status -> Html
view status =
    let
        main = withDefault "Ready" status.main
        rollover = withDefault "Dummy" status.rollover
    in
        div [ id "status" ]
        [ div [ id "main" ] [ main |> text ]
        , div [ id "rollover" ] [ rollover |> text ]
        ]

