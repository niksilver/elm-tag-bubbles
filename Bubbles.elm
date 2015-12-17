module Bubbles where

import Html exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model = { x : Float, y : Float }

type Action = Empty

update : Action -> Model -> Model
update action model =
    model

view : Signal.Address Action -> Model -> Form
view address model =
    circle 180
        |> filled darkBrown
        |> move (model.x, model.y)

