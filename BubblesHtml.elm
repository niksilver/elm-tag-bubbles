module BubblesHtml where

-- Bubble forms captured as Html

import Bubble exposing (..)

import Html exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model =
    { width : Int
    , height : Int
    , bubble : Bubble.Model
    }

type Action = Resize (Int, Int)

update : Action -> Model -> Model
update action model =
    case action of
        Resize (w, h) ->
            { model | width = w, height = h }

view : Signal.Address Action -> Model -> Html
view address model =
    collage model.width model.height
    [ Bubble.view address model.bubble
    ]
        |> fromElement

