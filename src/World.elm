module World
    ( Model
    , Action (Tick, NewTags, Recentre, Resize, Scale)
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
import Html exposing (Html)
import Html.Attributes exposing (id)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height)
import Time exposing (Time)

type alias Model =
    { bubbles : MB.Model
    , springs : Dict (Id,Id) Float
    -- Maybe the system has told us our dimensions
    , dimensions : Maybe (Int, Int)
    }

type Action
    = Direct MB.Action
    | Tick Time
    | NewTags (List Tags)
    | Recentre
    | Resize (Int, Int)
    | Scale String

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
            case model.dimensions of
                Just dimensions -> newTags listListTag dimensions model
                Nothing -> model
        Recentre ->
            case model.dimensions of
                Just dims ->
                    { model | bubbles = MB.recentre model.bubbles dims }
                Nothing ->
                    model
        Resize windowDims ->
            let
                newDims = size windowDims
                newBubbles =
                    case model.dimensions of
                        Just oldDims ->
                            MB.forNewDimensions oldDims newDims model.bubbles
                        Nothing ->
                            MB.recentre model.bubbles newDims
            in
            { model
            | dimensions = Just newDims
            , bubbles = newBubbles
            }
        Scale scaleStr ->
            let
                s = Debug.log "scale" scaleStr
            in
                model

-- Introduce new tags to the model

newTags : List Tags -> (Int, Int) -> Model -> Model
newTags listListTag (width, height) model =
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
        | bubbles =
            MB.initialArrangement
            (toFloat width / 2)
            (toFloat height / 2)
            model.bubbles
            bubbles
        , springs = springs
        }
 
-- Work out the size of the world based on the window's dimensions

size : (Int, Int) -> (Int, Int)
size (winWidth, winHeight) =
    let
        -- World's width with borders
        fullyBorderedWidth = winWidth - 2 * Constants.sideBorderWidth
        -- Reduced borders if less than the preferred min width
        idealWidth = max fullyBorderedWidth Constants.minWorldWidth
        -- Constrained to window width
        width = min winWidth idealWidth
        height = winHeight - Constants.navHeight - Constants.statusBarHeight
    in
        (width, height)

-- The view
-- The world might not yet have got its dimensions, so we have two cases

view : Context Action -> Model -> Html
view context model =
    case model.dimensions of
        Just dims -> viewWithDimensions context dims model
        Nothing -> viewNoDimensions

viewNoDimensions : Html
viewNoDimensions =
    svg [] []

viewWithDimensions : Context Action -> (Int, Int) -> Model -> Html
viewWithDimensions context (wdth, hght) model =
    svg
        [ id "world"
        , width (wdth |> toString)
        , height (hght |> toString)
        ]
        (MB.view (forwardTo context Direct) model.bubbles)

