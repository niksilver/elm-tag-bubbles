module BubblesHtml where

-- Bubble forms captured as Html

import Bubbles exposing (..)

import Html exposing (..)
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model =
    { width : Int
    , height : Int
    , bubbles : Bubbles.Model
    }

type Action = Bubble Bubbles.Action

update : Action -> Model -> Model
update action model =
    case action of
        Bubble act ->
            { model | bubbles = Bubbles.update act model.bubbles }

view : Signal.Address Action -> Model -> Html
view address model =
    collage model.width model.height
    [ Bubbles.view (Signal.forwardTo address Bubble) model.bubbles
    ]
        |> fromElement

