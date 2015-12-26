module BubblesHtml where

-- Bubble forms captured as Html

import Constants exposing (Tags)
import MultiBubbles as MB

import Html exposing (Html, div, text)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (forwardTo)
import Effects exposing (Effects)

type alias Model =
    { width : Int
    , height : Int
    , bubbles : MB.Model
    , newTags : List Tags
    }

type Action
    = Resize (Int, Int)
        | Direct MB.Action
        | Tick
        | NewTags (List Tags)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Resize (w, h) ->
            ({ model | width = w, height = h }
             , Effects.none
            )
        Direct act ->
            ({ model | bubbles = MB.update act model.bubbles }
             , Effects.none
            )
        Tick ->
            ({ model | bubbles = MB.update MB.Tick model.bubbles }
             , Effects.none
            )
        NewTags tagsList ->
            ({ model | newTags = tagsList }
             , Effects.none
            )

view : Signal.Address Action -> Model -> Html
view address model =
    div []
    [ svgView address model
    , text (toString model.newTags)
    ]

svgView : Signal.Address Action -> Model -> Html
svgView address model =
    svg
        [ width (toString model.width)
        , height (toString model.height)
        ]
        (MB.view (forwardTo address Direct) model.bubbles)

