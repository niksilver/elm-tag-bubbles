module Bubble
    ( Model
    , Action (Move, Fade)
    , noAnimation, fadeInAnimation, setToFadeIn, setToFadeOut, isFadedOut
    , update, view
    ) where

import Constants exposing
    ( bubbleLabelFontSize
    , maxBubbleOpacity
    , fadeDuration
    )
import Context exposing (Context)
import Colours exposing (pickBaseColour, pickTextColour)
import Time exposing (Time)

import Maybe exposing (withDefault)
import Svg exposing 
    ( Svg
    , circle, text, text', foreignObject, g
    )
import Svg.Attributes exposing
    ( cx, cy, r, fill, opacity
    , x, y, textAnchor, alignmentBaseline, fontSize
    )
import Html
import Html.Attributes
import Html.Events
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

setToFadeOut : Model -> Model
setToFadeOut model =
    let
        animation = model.animation
        fromOpacity =
            case animation.fading of
                Fading from to -> animation.opacity
                NotFading -> maxBubbleOpacity
    in
        { model
        | animation =
            { animation
            | fadeStart = Nothing
            , fading = Fading fromOpacity 0.0
            }
        }

isFadedOut : Model -> Bool
isFadedOut model =
    case model.animation.fading of
        Fading from to ->
            (to == 0.0) && (model.animation.opacity == 0.0)
        NotFading ->
            False
 
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
        workaroundForChromeTextClick =
            Html.Events.onClick context.click model.id
        textDiv =
            Html.div
            [ Html.Attributes.style
                [ ("position", "relative")
                , ("top", "50%")
                , ("transform", "translateY(-50%)")
                , ("text-align", "center")
                , ("color", (pickTextColour model.label))
                , ("font-family", "Arial, Helvetica, sans-serif")
                , ("opacity", (model.animation.opacity |> toString))
                ]
            , workaroundForChromeTextClick
            ]
            [ text model.label ]
        foreignObjectAttrs =
            [ Svg.Attributes.x (toString (model.x - model.size * 0.85))
            , Svg.Attributes.y (toString (model.y - model.size * 0.40))
            , Svg.Attributes.width  (toString (model.size * 2 * 0.85))
            , Svg.Attributes.height (toString (model.size * 2 * 0.40))
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
    in
        g []
        [ circle baseCircleAttrs []
        , foreignObject foreignObjectAttrs [ textDiv ]
        , circle coveringCircleAttrs' []
        ]

