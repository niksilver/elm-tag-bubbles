module BubblesHtml where

-- Bubble forms captured as Html

import MultiBubbles as MB

import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (forwardTo)
import Effects exposing (Effects)

type alias Model =
    { width : Int
    , height : Int
    , bubbles : MB.Model
    }

type Action
    = Resize (Int, Int)
        | Direct MB.Action
        | Tick

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Resize (w, h) ->
            ({ model | width = w, height = h }, Effects.none)
        Direct act ->
            ({ model | bubbles = MB.update act model.bubbles }, Effects.none)
        Tick ->
            ({ model | bubbles = MB.update MB.Tick model.bubbles }, Effects.none)

viewBoxStr : Model -> String
viewBoxStr model =
    "0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)

view : Signal.Address Action -> Model -> Html
view address model =
    svg
        [ width (toString model.width)
        , height (toString model.height)
        , viewBox (viewBoxStr model)
        ]
        (MB.view (forwardTo address Direct) model.bubbles)

