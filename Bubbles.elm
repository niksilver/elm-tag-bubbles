module Bubbles where

import Html exposing (..)

type alias Model = { x : Int, y : Int }

type Action = Empty

update : Action -> Model -> Model
update action model =
        model

view : Signal.Address Action -> Model -> Html
view address model =
        div [] [ text "Cest ne pas un bubble" ]
