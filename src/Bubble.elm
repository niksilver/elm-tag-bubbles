module Bubble where

import Constants exposing (colour1, colour2, bubbleOpacity)

import Svg exposing (Svg, circle, text, text', g)
import Svg.Attributes exposing
    ( cx, cy, r, fill, opacity
    , x, y, textAnchor, alignmentBaseline, fontSize
    )
import Svg.Events exposing (onClick)
import Signal exposing (message)

type alias Model =
    { id : String
    , x : Float
    , y : Float
    , size : Float
    , label : String
    , colour: String
    }

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
    let
        circleAttrs =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , fill model.colour
            , opacity bubbleOpacity
            , onClick (message address Flip)
            ]
        textAttrs =
            [ x (toString model.x)
            , y (toString model.y)
            , textAnchor "middle"
            -- This next one doesn't work in Firefox
            , alignmentBaseline "central"
            , fontSize "40pt"
            ]
    in
        g []
        [ circle circleAttrs []
        , text' textAttrs [ text model.label ]
        ]

