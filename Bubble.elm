module Bubble where

import Html exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model = { x : Float, y : Float, size : Float }

update : a -> Model -> Model
update action model =
    model

view : Signal.Address a -> Model -> Form
view address model =
    circle model.size
        |> filled red
        |> move (model.x, model.y)

