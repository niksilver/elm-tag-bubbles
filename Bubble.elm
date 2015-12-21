module Bubble where

import Color exposing (..)
import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill)

type alias Model = { x : Float, y : Float, size : Float }

update : a -> Model -> Model
update action model =
    model

view : Signal.Address a -> Model -> Svg
view address model =
    circle
        [ cx (toString model.x)
        , cy (toString model.y)
        , r (toString model.size)
        , fill "green"
        ] []

