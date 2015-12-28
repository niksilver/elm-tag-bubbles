module Bubble where

import Constants exposing (colour1, colour2, bubbleOpacity)

import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, opacity)
import Svg.Events exposing (onClick)
import Signal exposing (message)

type alias Model =
    { id : String
    , x : Float
    , y : Float
    , size : Float
    , colour: String}

type Action = Flip | Move Float Float

update : Action -> Model -> Model
update action model =
    case action of
        Flip -> flip model
        Move dx dy -> { model | x = model.x + dx, y = model.y + dy }

flip : Model -> Model
flip model =
    if model.colour == colour1 then
        { model | colour = colour2 }
    else
        { model | colour = colour1 }

view : Signal.Address Action -> Model -> Svg
view address model =
    circle
        [ cx (toString model.x)
        , cy (toString model.y)
        , r (toString model.size)
        , fill model.colour
        , opacity bubbleOpacity
        , onClick (message address Flip)
        ] []

