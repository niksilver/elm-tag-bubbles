module World
    ( Model
    , Action (Tick, NewTags)
    , update, view
    ) where

import Constants exposing
    ( Id, Tags
    , maxBubbles
    , minBubbleSize, maxBubbleSize
    , minSpringLength, maxSpringLength
    , springStrength)
import Context exposing (Context, forwardTo)
import MultiBubbles as MB
import Bubble exposing (fadeInAnimation)
import Springs exposing (toCounter, acceleration, accelDict)
import Sizes
import TagFetcher

import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Time exposing (Time)

type alias Model =
    { bubbles : MB.Model
    , springs : Dict (Id,Id) Float
    }

type Action
    = Direct MB.Action
    | Tick Time
    | NewTags (List Tags)

update : Action -> Model -> Model
update action model =
    case action of
        Direct act -> { model | bubbles = MB.update act model.bubbles }
        Tick time ->
            let
                strength = springStrength
                accelFn = acceleration strength model.springs
                bubbles = List.map .bubble model.bubbles
                accels = accelDict bubbles accelFn
                bubsMod = MB.update (MB.AdjustVelocities accels) model.bubbles
                model' = { model | bubbles = bubsMod }
            in
                update (Direct (MB.Tick time)) model'
        NewTags listListTag ->
            let
                topN = Sizes.topN maxBubbles listListTag
                listListTag' = Sizes.filter listListTag topN
                tags = Sizes.idDict listListTag' |> Dict.values
                pairCounter = Springs.toCounter listListTag'
                springs = Springs.toDict minSpringLength maxSpringLength pairCounter
                sizes = Sizes.toDict listListTag'
                    |> Sizes.rescale minBubbleSize maxBubbleSize
                makePhysBubble tag =
                    { dx = 0.0
                    , dy = 0.0
                    , bubble =
                        { id = tag.id
                        , x = 0.0
                        , y = 0.0
                        , size = Dict.get tag.id sizes |> withDefault 10.0
                        , label = tag.webTitle
                        , animation = fadeInAnimation
                        }
                    }
                physBubbles = List.map makePhysBubble tags
                physBubbles' = MB.initialModel 400 300 physBubbles
            in
                { bubbles = MB.replace model.bubbles physBubbles'
                , springs = springs
                }

view : Context Action -> Model -> List Svg
view context model =
    MB.view (forwardTo context Direct) model.bubbles

