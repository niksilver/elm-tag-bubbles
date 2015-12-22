module Bubble where

import Color exposing (..)
import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, opacity)
import Svg.Events exposing (onClick)
import Signal exposing (message)

type alias Model = { x : Float, y : Float, size : Float, colour: String}

update : a -> Model -> Model
update action model =
    flip model

type Action = Flip

flip : Model -> Model
flip model =
    if model.colour == "green" then
        { model | colour = "red" }
    else
        { model | colour = "green" }

view : Signal.Address Action -> Model -> Svg
view address model =
    circle
        [ cx (toString model.x)
        , cy (toString model.y)
        , r (toString model.size)
        , fill model.colour
        , opacity "0.75"
        , onClick (message address Flip)
        ] []

