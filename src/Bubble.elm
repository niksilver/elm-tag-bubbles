module Bubble
    ( Model
    , Action (Move, Fade)
    , noAnimation, fadeInAnimation, setToFadeIn
    , update, view
    ) where

import Constants exposing (bubbleLabelFontSize, maxBubbleOpacity, fadeDuration)
import Context exposing (Context)
import Colours exposing (pickBaseColour, pickTextColour)
import Time exposing (Time)

import Maybe exposing (withDefault)
import Svg exposing 
    ( Svg
    , circle, text, text', g
    )
import Svg.Attributes exposing
    ( cx, cy, r, fill, opacity
    , x, y, textAnchor, alignmentBaseline, fontSize
    )
import Html
import Svg.Events exposing (onClick)
import Signal exposing (message)
import Easing exposing (ease, linear, float)

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
    , opacity : Float         -- Current opacity
    }

-- Fading from and to an opacity, or not fading
type Fading = Fading Float Float | NotFading

type Action
    = Move Float Float
        | Fade Time

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
    case action of
        Fade time ->
            { model
            | animation = updateFade model.animation time
            }
        Move dx dy ->
            { model | x = model.x + dx, y = model.y + dy }

updateFade : Animation -> Time -> Animation
updateFade animation time =
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
        fo =
            Svg.foreignObject
            [ Svg.Attributes.x (toString (model.x - model.size * 0.85))
            , Svg.Attributes.y (toString (model.y - model.size * 0.35))
            , Svg.Attributes.width  (toString (model.size * 2 * 0.85))
            , Svg.Attributes.height (toString (model.size * 2 * 0.35))
            , opacity (model.animation.opacity |> toString)
            ]
            [
                Html.div [] [ text model.label ]
            ]
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
            , fontSize bubbleLabelFontSize
            , fill (pickTextColour model.label)
            ]
    in
        g []
        [ circle baseCircleAttrs []
        -- , text' textAttrs [ text model.label ]
        , fo
        , circle coveringCircleAttrs' []
        ]

