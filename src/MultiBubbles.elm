module MultiBubbles
    ( Action (Tick, Direct, AdjustVelocities)
    , Model
    , Diff, tagDiff
    , initialArrangement
    , update
    , view
    ) where

-- Multiple bubbles

import Constants exposing (Id, Tag, Tags)
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

-- Reorder the bubbles with the largest first

reorder : Model -> Model
reorder model =
    model
        |> List.sortBy (\bubble -> -1 * (bubble.size))

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

removeFadedOut : Model -> Model
removeFadedOut model =
    filter (Bubble.isFadedOut >> not) model

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

