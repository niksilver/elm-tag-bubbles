module World
    ( Model
    , Action (Tick, NewTags, Recentre, Resize)
    , size
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
    , dimensions : (Int, Int)
    }

type Action
    = Direct MB.Action
    | Tick Time
    | NewTags (List Tags)
    | Recentre (Int, Int)
    | Resize (Int, Int)

update : Action -> Model -> Model
update action model =
    case action of
        Direct act -> { model | bubbles = MB.update act model.bubbles }
        Tick time ->
            let
                strength = springStrength
                accelFn = acceleration strength model.springs
                bubbles = model.bubbles
                accels = accelDict bubbles accelFn
                bubsMod = MB.update (MB.AdjustVelocities accels) bubbles
                model' = { model | bubbles = bubsMod }
            in
                update (Direct (MB.Tick time)) model'
        NewTags listListTag ->
            let
                listListTag' =
                    listListTag
                        |> Sizes.topN maxBubbles
                        |> Sizes.filter listListTag
                springs =
                    Springs.toCounter listListTag'
                        |> Springs.toDictWithZeros minSpringLength maxSpringLength
                tags = Sizes.idDict listListTag' |> Dict.values
                bubbles =
                    Sizes.toDict listListTag'
                        |> Sizes.rescale minBubbleSize maxBubbleSize
                        |> MB.make tags
            in
                { model
                | bubbles = MB.initialArrangement 400 300 model.bubbles bubbles
                , springs = springs
                }
        Recentre dims ->
            { model | bubbles = MB.recentre model.bubbles dims }
        Resize windowDims ->
            { model | dimensions = size windowDims }

-- Work out the size of the world based on the window's dimensions

size : (Int, Int) -> (Int, Int)
size (winWidth, winHeight) =
    let
        width = min winWidth (winWidth)
        height = winHeight - 100
    in
        (width, height)

-- The view

view : Context Action -> Model -> List Svg
view context model =
    MB.view (forwardTo context Direct) model.bubbles

