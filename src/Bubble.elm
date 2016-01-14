module Bubble
    ( Model
    , Action (Move, Fade, MarkTime)
    , noAnimation, fadeInAnimation, setToFadeIn
    , update, view
    ) where

import Constants exposing (maxBubbleOpacity, fadeDuration)
import Context exposing (Context)
import Colours exposing (pickBaseColour, pickTextColour)
import Time exposing (Time)

import Maybe exposing (withDefault)
import Svg exposing (Svg, circle, text, text', g)
import Svg.Attributes exposing
    ( cx, cy, r, fill, opacity
    , x, y, textAnchor, alignmentBaseline, fontSize
    )
import Svg.Events exposing (onClick)
import Signal exposing (message)
import Easing exposing (ease, linear, float)
import Debug

type alias Model =
    { id : String
    , x : Float
    , y : Float
    , size : Float
    , label : String
    , animation : Animation
    }

type alias Animation =
    { fadeStart : Maybe Time  -- Time if the fading has started
    , fading : Fading         -- How it's fading, if it is to fade
    , opacity : Float
    }

-- Fading from and to an opacity, or not fading
type Fading = Fading Float Float | NotFading

type Action
    = Move Float Float
        | Fade Time
        | MarkTime Time

noAnimation : Animation
noAnimation =
    { fadeStart = Nothing
    , fading = NotFading
    , opacity = maxBubbleOpacity
    }

fadeInAnimation : Animation
fadeInAnimation =
    { fadeStart = Nothing
    , fading = Fading 0.0 maxBubbleOpacity
    , opacity = 0.0
    }

setToFadeIn : Model -> Model
setToFadeIn model =
    let
        animation = model.animation
        fromOpacity =
            case animation.fading of
                Fading from to -> animation.opacity
                NotFading -> 0.0
    in
        { model
        | animation =
            { animation
            | fadeStart = Nothing
            , fading = Fading fromOpacity maxBubbleOpacity
            }
        }

update : Action -> Model -> Model
update action model =
    let
        func label = (if (model.id == "uk/uk") then Debug.log label else identity)
    in
    case action of
        Fade time ->
            { model
            | animation = updateFade model.id model.animation time
            }
        Move dx dy ->
            { model | x = model.x + dx, y = model.y + dy }
        MarkTime time ->
            (func "MarkTime") model

updateFade : String -> Animation -> Time -> Animation
updateFade id animation time =
    case animation.fading of
        NotFading -> animation
        Fading from to ->
            let
                fadeStart = animation.fadeStart |> withDefault time
                elapsed = time - fadeStart
                opacity = ease linear float from to fadeDuration elapsed
            in
                { animation
                | fadeStart = Just fadeStart
                , opacity = opacity
                }

view : Context Action -> Model -> Svg
view context model =
    let
        baseCircleAttrs =
            [ cx (toString model.x)
            , cy (toString model.y)
            , r (toString model.size)
            , fill (pickBaseColour model.label)
            , opacity (model.animation.opacity |> toString)
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
            , opacity (model.animation.opacity |> toString)
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

