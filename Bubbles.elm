module Bubbles where

import Html exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model = { x : Int, y : Int }

type Action = Empty

update : Action -> Model -> Model
update action model =
    model

view : Signal.Address Action -> Model -> Html
view address model =
    collage 300 300
    [ circle 200
        |> filled darkBrown
        |> move (100, 100)
    ]
    |> fromElement

