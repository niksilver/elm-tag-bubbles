module PhysicsBubble where

import Bubble exposing (Model)

import Svg exposing (Svg)
import Signal

type alias Model =
    { dx : Float
    , dy : Float
    , bubble : Bubble.Model
    }

type Action = Move | Direct Bubble.Action

update : Action -> Model -> Model
update action model =
    let
        act : Bubble.Action
        act =
            case action of
                Move -> Bubble.Move model.dx model.dy
                Direct a -> a
    in
        { model | bubble = Bubble.update act model.bubble }

view : Signal.Address Action -> Model -> Svg
view address model =
    Bubble.view (Signal.forwardTo address Direct) model.bubble

