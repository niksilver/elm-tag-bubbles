module MultiBubbles
    ( Action (Tick, Direct, AdjustVelocities)
    , Model
    , Diff, tagDiff
    , makeBubbles
    , initialArrangement
    , update
    , view
    ) where

-- Multiple bubbles

import Constants exposing (Id, Tag)
import Context exposing (Context, forwardTo)
import Bubble
import Springs exposing (drag, dampen)

import List exposing (reverse, length, indexedMap, map, append, filter, member)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Svg exposing (Svg)
import Time exposing (Time)

type alias Model = List Bubble.Model

type Action
    = Tick Time
    | Direct Id Bubble.Action
    | AdjustVelocities (Dict Id (Float, Float))

-- The difference between tags already in the world and tags fetched
-- via the API.

type alias Diff a = { old : List a, new : List a, both : List a }

-- Find the difference between tags represented in the world
-- and tags fetched by the API

tagDiff : List a -> List a -> Diff a
tagDiff current latest =
    let
        old = filter (\e -> not(member e latest)) current
        new = filter (\e -> not(member e current)) latest
        both = filter (\e -> member e current) latest
    in
        Diff old new both

-- Make some new bubbles using a dictionary of tags and a dictionary of sizes

makeBubbles : List Tag -> Dict Id Float -> Model
makeBubbles tags sizeDict =
    let
        size tag = Dict.get tag.id sizeDict |> withDefault 10.0
        mkBubble tag = Bubble.makeBubble tag (size tag)
    in
        List.map mkBubble tags

-- Create an initial arrangement for a number of new bubbles.
-- Old bubbles will fade out; new bubbles will fade in; remaining bubbles remain

initialArrangement : Float -> Float -> Model -> Model -> Model
initialArrangement centreX centreY oldModel newModel =
    arrangeCentre centreX centreY newModel
        |> replace oldModel
        |> reorder

arrangeCentre : Float -> Float -> Model -> Model
arrangeCentre centreX centreY model =
    let
        count = length model
        turn = 2 * pi / (toFloat count)
        rePos : Int -> Bubble.Model -> Bubble.Model
        rePos idx bubble =
            { bubble
            | x = centreX + 40 * (cos (turn * (toFloat idx)))
            , y = centreY + 40 * (sin (turn * (toFloat idx)))
            }
                |> Bubble.setToFadeIn
        indexedBubs = indexedMap (,) model
    in
        map (\ib -> rePos (fst ib) (snd ib)) indexedBubs

-- Replace an old model with a new model.

replace : Model -> Model -> Model
replace oldModel newModel =
    let
        diff = tagDiff (map .id oldModel) (map .id newModel)
    in
        oldModel
            |> fadeIn newModel diff.new
            |> fadeOut diff.old
            |> resize newModel diff.both

fadeIn : Model -> List Id -> Model -> Model
fadeIn newModel newIds oldModel =
    filter (\bubble -> member bubble.id newIds) newModel
        |> append oldModel

fadeOut : List Id -> Model -> Model
fadeOut oldIds oldModel =
    let
        selectedFade bubble =
            if (member bubble.id oldIds) then
                Bubble.setToFadeOut bubble
            else
                bubble
    in
        map selectedFade oldModel

resize : Model -> List Id -> Model -> Model
resize newModel ids model =
    let
        newBubbleSize id =
            find (\b -> b.id == id) newModel
                |> Maybe.map .size
                |> withDefault 10.0
        selectedResize bubble =
            if (member bubble.id ids) then
                Bubble.setToResize (newBubbleSize bubble.id) bubble
            else
                bubble
    in
        map selectedResize model

-- Find the first element in a list that passes a given test

find : (a -> Bool) -> List a -> Maybe a
find test list =
    List.filter test list |> List.head

-- Reorder the bubbles with the largest first

reorder : Model -> Model
reorder model =
    model
        |> List.sortBy (\bubble -> -1 * (Bubble.targetSize bubble))

-- Update the model

update : Action -> Model -> Model
update action model =
    case action of
        Direct id bubAct -> updateOne id bubAct model
        Tick time -> updateAll model time
        AdjustVelocities accels -> updateVelocities accels model

updateOne : Id -> Bubble.Action -> Model -> Model
updateOne id bubAct model =
    let
        selectiveUpdate bubModel =
            if id == bubModel.id then
                (Bubble.update bubAct bubModel)
            else
                bubModel
    in
        map selectiveUpdate model

-- Update all the bubbles with the Tick action

updateAll : Model -> Time -> Model
updateAll model time =
    map (Bubble.update (Bubble.Animate time)) model
        |> removeFadedOut
        |> cancelFinishedResizes

removeFadedOut : Model -> Model
removeFadedOut model =
    filter (Bubble.isFadedOut >> not) model

cancelFinishedResizes : Model -> Model
cancelFinishedResizes model =
    map Bubble.cancelFinishedResize model

-- Update the velocity of all the bubbles
-- according to a dictionary of id to acceleration

updateVelocities : Dict Id (Float, Float) -> Model -> Model
updateVelocities accels model =
    map (updateVelocity accels) model

updateVelocity : Dict Id (Float, Float) -> Bubble.Model -> Bubble.Model
updateVelocity accels bubble =
    let
        accelXY = Dict.get bubble.id accels |> withDefault (0, 0)
        accelX = fst accelXY
        accelY = snd accelXY
        dragXY = drag bubble.dx bubble.dy
        dragX = fst dragXY
        dragY = snd dragXY
        dx = bubble.dx + accelX + dragX
        dy = bubble.dy + accelY + dragY
        dampenedDxDy = dampen dx dy
    in
        { bubble | dx = fst dampenedDxDy, dy = snd dampenedDxDy }

-- A view of a Bubble, using an address at this level of the architecture

fwdingView : Context Action -> Bubble.Model -> Svg
fwdingView context bubble =
    Bubble.view (forwardTo context (Direct bubble.id)) bubble

view : Context Action -> Model -> List Svg
view context model =
    map (fwdingView context) model

