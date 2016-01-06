module PhysicsBubble where

import Context exposing (Context, forwardTo)
import Bubble exposing (Model)

import Svg exposing (Svg)

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

view : Context Action -> Model -> Svg
view context model =
    Bubble.view (forwardTo context Direct) model.bubble

