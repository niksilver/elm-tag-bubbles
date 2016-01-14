module PhysicsBubble where

import Context exposing (Context, forwardTo)
import Bubble exposing (Model)

import Svg exposing (Svg)
import Time exposing (Time)

type alias Model =
    { dx : Float
    , dy : Float
    , bubble : Bubble.Model
    }

type Action = Animate Time | Direct Bubble.Action

update : Action -> Model -> Model
update action model =
    case action of
        Direct act ->
            { model
            | bubble = Bubble.update act model.bubble
            }
        Animate time ->
            { model
            | bubble =
                model.bubble
                    |> Bubble.update (Bubble.Move model.dx model.dy)
                    |> Bubble.update (Bubble.Fade time)
            }

view : Context Action -> Model -> Svg
view context model =
    Bubble.view (forwardTo context Direct) model.bubble

