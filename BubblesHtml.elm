module BubblesHtml where

-- Bubble forms captured as Html

import Bubble exposing (Action(Flip))

import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (forwardTo)

type alias Model =
    { width : Int
    , height : Int
    , bubble : Bubble.Model
    }

type Action = Resize (Int, Int) | OneOf Bubble.Action

update : Action -> Model -> Model
update action model =
    case action of
        Resize (w, h) ->
            { model | width = w, height = h }
        OneOf Flip ->
            { model | bubble = Bubble.update Flip model.bubble }

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
        [ Bubble.view (forwardTo address OneOf) model.bubble
        ]

