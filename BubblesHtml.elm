module BubblesHtml where

-- Bubble forms captured as Html

import Bubbles exposing (Action)

import Html exposing (..)
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model =
    { width : Int
    , height : Int
    , bubblesModel : Bubbles.Model
    }

update : Action -> Model -> Model
update action model =
    model

view : Signal.Address Action -> Model -> Html
view address model =
    collage model.width model.height
    [ Bubbles.view (Signal.forwardTo address (\x -> Bubbles.Empty)) model.bubblesModel
    ]
        |> fromElement

