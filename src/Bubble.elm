module Bubble where

import Constants exposing (colour1, colour2, bubbleOpacity)
import Context exposing (Context)
import Colours exposing (pickBaseColour, pickTextColour)

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
    }

type Action = Click | Move Float Float

update : Action -> Model -> Model
update action model =
    case action of
        Click -> flip model
        Move dx dy -> { model | x = model.x + dx, y = model.y + dy }

flip : Model -> Model
flip model =
    if model.label == "Flipped" then
        { model | label = "Unflipped" }
    else
        { model | label = "Flipped" }

view : Context Action -> Model -> Svg
view context model =
    let
        baseCircleAttrs =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , fill (pickBaseColour model.label)
            , opacity bubbleOpacity
            ]
        coveringCircleAttrs' =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , opacity "0"
            , onClick (message context.click Click)
            ]
        textAttrs =
            [ x (toString model.x)
            , y (toString model.y)
            , textAnchor "middle"
            -- This next one doesn't work in Firefox
            , alignmentBaseline "central"
            , fontSize "40pt"
            , fill (pickTextColour model.label)
            ]
    in
        g []
        [ circle baseCircleAttrs []
        , text' textAttrs [ text model.label ]
        , circle coveringCircleAttrs' []
        ]

