module Bubble
    ( Model
    , Fading (In, Out, None)
    , Action (Move, Fade)
    , update, view
    ) where

import Constants exposing (maxBubbleOpacity, fadeStep)
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
    , fading : Fading
    , opacity : Float
    }

type Fading = In | Out | None

type Action = Move Float Float | Fade

update : Action -> Model -> Model
update action model =
    case action of
        Fade ->
            { model
            | opacity = stepOpacity model
            , fading = stepFade model
            }
        Move dx dy -> { model | x = model.x + dx, y = model.y + dy }

stepOpacity : Model -> Float
stepOpacity model =
    case model.fading of
        In ->
            min (model.opacity + fadeStep) maxBubbleOpacity
        Out ->
            max (model.opacity - fadeStep) 0.0
        None ->
            model.opacity

stepFade : Model -> Fading
stepFade model =
    if (model.fading == In && model.opacity + fadeStep >= maxBubbleOpacity) then
        None
    else if (model.fading == Out && model.opacity - fadeStep <= 0.0) then
        None
    else
        model.fading

view : Context Action -> Model -> Svg
view context model =
    let
        baseCircleAttrs =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , fill (pickBaseColour model.label)
            , opacity (toString model.opacity)
            ]
        coveringCircleAttrs' =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , opacity "0"
            , onClick (message context.click model.id)
            ]
        textAttrs =
            [ x (toString model.x)
            , y (toString model.y)
            , textAnchor "middle"
            , opacity (toString model.opacity)
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

