module World exposing
    ( Model
    , Msg (..)
    , ViewBox
    , size, viewBox, viewBoxToString
    , update, view
    )


import Constants exposing
    ( Id, Tags
    , maxBubbles
    , minBubbleSize, maxBubbleSize
    , minSpringLength, maxSpringLength
    , springStrength)
-- import Context exposing (Context, forwardTo)
import MultiBubbles as MB
import Bubble exposing (fadeInAnimation)
import Springs exposing (toCounter, acceleration, accelDict)
import Sizes
import TagFetcher

import String
import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (id)
import Svg exposing (svg)
import Svg.Attributes as SVGA exposing (width, height, viewBox)
import Time exposing (Posix)


type alias Model =
    { bubbles : MB.Model
    , springs : Dict (Id,Id) Float
    -- Maybe the system has told us our dimensions
    , dimensions : Maybe (Int, Int)
    , scale : Float
    }


type Msg
    = Direct MB.Msg
    | Tick Posix
    -- | NewTags (List Tags)
    -- | Recentre
    -- | Resize (Int, Int)
    -- | Scale String


type alias ViewBox
    = { minX : Float
      , minY : Float
      , width : Float
      , height : Float }


update : Msg -> Model -> Model
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
                model_ = { model | bubbles = bubsMod }
            in
                update (Direct (MB.Tick time)) model_
--         NewTags listListTag ->
--             case model.dimensions of
--                 Just dimensions -> newTags listListTag dimensions model
--                 Nothing -> model
--         Recentre ->
--             case model.dimensions of
--                 Just dims ->
--                     { model | bubbles = MB.recentre model.bubbles dims }
--                 Nothing ->
--                     model
--         Resize windowDims ->
--             let
--                 newDims = size windowDims
--                 newBubbles =
--                     case model.dimensions of
--                         Just oldDims ->
--                             MB.forNewDimensions oldDims newDims model.bubbles
--                         Nothing ->
--                             MB.recentre model.bubbles newDims
--             in
--             { model
--             | dimensions = Just newDims
--             , bubbles = newBubbles
--             }
--         Scale scaleStr ->
--             case String.toFloat (Debug.log "scale" scaleStr) of
--                 Ok scale -> { model | scale = scale }
--                 Err err -> model


-- Introduce new tags to the model

newTags : List Tags -> (Int, Int) -> Model -> Model
newTags listListTag (width, height) model =
    let
        listListTag_ =
            listListTag
                |> Sizes.topN maxBubbles
                |> Sizes.filter listListTag
        springs =
            Springs.toCounter listListTag_
                |> Springs.toDictWithZeros minSpringLength maxSpringLength
        tags = Sizes.idDict listListTag_ |> Dict.values
        bubbles =
            Sizes.toDict listListTag_
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


-- Calculating values for an svg view box

viewBox : (Int, Int) -> Float -> ViewBox
viewBox dims scale =
    let
        winWidth = Tuple.first dims |> toFloat
        winHeight = Tuple.second dims |> toFloat
        minX = 0.5 * (winWidth - winWidth / scale)
        minY = 0.5 * (winHeight - winHeight / scale)
        width = winWidth / scale
        height = winHeight / scale
    in
        ViewBox minX minY width height


viewBoxToString : ViewBox -> String
viewBoxToString vb =
    String.fromFloat vb.minX ++ " " ++
    String.fromFloat vb.minY ++ " " ++
    String.fromFloat vb.width ++ " " ++
    String.fromFloat vb.height


-- The view
-- The world might not yet have got its dimensions, so we have two cases

view : Never -> Model -> Html Never
view context_removed model =
    case model.dimensions of
        Just dims -> viewWithDimensions context_removed dims model
        Nothing -> viewNoDimensions


viewNoDimensions : Html Never
viewNoDimensions =
    svg [] []


viewWithDimensions : Never -> (Int, Int) -> Model -> Html Never
viewWithDimensions context_removed (wdth, hght) model =
    svg
        [ id "world"
        , width (wdth |> String.fromInt)
        , height (hght |> String.fromInt)
        , SVGA.viewBox (viewBox (wdth, hght) model.scale |> viewBoxToString)
        ]
        -- (MB.view (forwardTo context Direct) model.bubbles)
        (MB.view context_removed model.bubbles)

