module Status exposing
  ( Status
  , Msg(..), update
  , view
  )

import Constants

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class, style)
import Maybe exposing (withDefault)


type alias Status = { rollover : Maybe String, main : Maybe String }


type Msg
  = Rollover String
  | NoRollover
  | Main String
  | NoMain


-- Update the status

update : Msg -> Status -> Status
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

view : Status -> Html a
view status =
  let
      main = withDefault "Ready" status.main
      rollover = withDefault "" status.rollover
  in
      div
        [ id "status"
        , style "height" (String.fromInt Constants.statusBarHeight ++ "px")
        , style "line-height" (String.fromInt Constants.statusBarHeight ++ "px")
        ]
        [ div [ class "inner-status" ] [ main |> text ]
        , div [ class "inner-status" ] [ rollover |> text ]
        ]

