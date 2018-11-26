module Status
    ( Status
    , Action(Rollover, NoRollover, Main, NoMain), update
    , view
    ) where

import Constants

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class, style)
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
        rollover = withDefault "" status.rollover
    in
        div
        [ id "status"
        , style
          [ ("height", toString Constants.statusBarHeight ++ "px")
          , ("line-height", toString Constants.statusBarHeight ++ "px")
          ]
        ]
        [ div [ class "inner-status" ] [ main |> text ]
        , div [ class "inner-status" ] [ rollover |> text ]
        ]

