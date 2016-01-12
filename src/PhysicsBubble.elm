module PhysicsBubble where

import Context exposing (Context, forwardTo)
import Bubble exposing (Model)

import Svg exposing (Svg)

type alias Model =
    { dx : Float
    , dy : Float
    , bubble : Bubble.Model
    }

type Action = Animate | Direct Bubble.Action

update : Action -> Model -> Model
update action model =
    case action of
        Direct act ->
            { model
            | bubble = Bubble.update act model.bubble
            }
        Animate ->
            { model
            | bubble =
                model.bubble
                    |> Bubble.update (Bubble.Move model.dx model.dy)
                    |> Bubble.update Bubble.Fade
            }

view : Context Action -> Model -> Svg
view context model =
    Bubble.view (forwardTo context Direct) model.bubble

