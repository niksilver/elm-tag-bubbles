module BubblesHtml where

-- Bubble forms captured as Html

import Bubbles exposing (..)

import Effects exposing (Effects)
import Html exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model =
    { width : Int
    , height : Int
    , bubbles : Bubbles.Model
    }

type Action = Resize (Int, Int) | Bubble Bubbles.Action

update : Action -> Model -> (Model, Effects action)
update action model =
    case action of
        Resize (w, h) ->
            let
                mod = { model | width = w, height = h }
                effect = Effects.none
            in
                (mod, effect)
        Bubble act ->
            let
                mod = { model | bubbles = Bubbles.update act model.bubbles }
                effect = Effects.none
            in
                (mod, effect)

view : Signal.Address Action -> Model -> Html
view address model =
    collage model.width model.height
    [ Bubbles.view (Signal.forwardTo address Bubble) model.bubbles
    ]
        |> fromElement

