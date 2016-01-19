module MultiBubbles
    ( Action (Tick, Direct, AdjustVelocities)
    , Model
    , Diff, tagDiff, replace
    , initialModel
    , update
    , view
    ) where

-- Multiple bubbles

import Constants exposing (Id, Tag, Tags)
import Context exposing (Context, forwardTo)
import PhysicsBubble as Phys
import Bubble
import Springs exposing (drag, dampen)

import List exposing (reverse, length, indexedMap, map)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Svg exposing (Svg)
import Time exposing (Time)

type alias Model = List Phys.Model

type Action
    = Tick Time
    | Direct Id Phys.Action
    | AdjustVelocities (Dict Id (Float, Float))

-- The difference between tags already in the world and tags fetched
-- via the API.

type alias Diff a = { old : List a, new : List a, both : List a }

-- Find the difference between tags represented in the world
-- and tags fetched by the API

tagDiff : List a -> List a -> Diff a
tagDiff current latest =
    let
        old = List.filter (\e -> not(List.member e latest)) current
        new = List.filter (\e -> not(List.member e current)) latest
        both = List.filter (\e -> List.member e current) latest
    in
        Diff old new both

-- Set initial model for multiple bubbles

initialModel : Float -> Float -> Model -> Model
initialModel centreX centreY model =
    let
        count = length model
        turn = 2 * pi / (toFloat count)
        rePos : Int -> Phys.Model -> Phys.Model
        rePos idx physBub =
            let
                pb = physBub.bubble
            in
                { physBub
                | bubble =
                    { pb
                    | x = centreX + 40 * (cos (turn * (toFloat idx)))
                    , y = centreY + 40 * (sin (turn * (toFloat idx)))
                    }
                        |> Bubble.setToFadeIn
                }
        indexedBubs = indexedMap (,) model
    in
        map (\ib -> rePos (fst ib) (snd ib)) indexedBubs

-- Replace an old model with a new model.
-- Old bubbles will fade out; new bubbles will fade in; remaining bubbles remain

replace : Model -> Model -> Model
replace oldModel newModel =
    let
        diff = tagDiff (map (.bubble >> .id) oldModel) (map (.bubble >> .id) newModel)
    in
        oldModel
            |> fadeIn newModel diff.new

fadeIn : Model -> List Id -> Model -> Model
fadeIn newModel newIds oldModel =
    List.filter (\pb -> List.member pb.bubble.id newIds) newModel
        |> List.append oldModel

-- Update the model

update : Action -> Model -> Model
update action model =
    case action of
        Direct id physAct -> updateOne id physAct model
        Tick time -> updateAll model time
        AdjustVelocities accels -> updateVelocities accels model

updateOne : Id -> Phys.Action -> Model -> Model
updateOne id physAct model =
    let
        selectiveUpdate physModel =
            if id == physModel.bubble.id then
                (Phys.update physAct physModel)
            else
                physModel
    in
        map selectiveUpdate model

-- Update all the bubbles with the Tick action

updateAll : Model -> Time -> Model
updateAll model time =
    map (Phys.update (Phys.Animate time)) model

-- Update the velocity of all the bubbles
-- according to a dictionary of id to acceleration

updateVelocities : Dict Id (Float, Float) -> Model -> Model
updateVelocities accels model =
    map (updateVelocity accels) model

updateVelocity : Dict Id (Float, Float) -> Phys.Model -> Phys.Model
updateVelocity accels physBub =
    let
        accelXY = Dict.get physBub.bubble.id accels |> withDefault (0, 0)
        accelX = fst accelXY
        accelY = snd accelXY
        dragXY = drag physBub.dx physBub.dy
        dragX = fst dragXY
        dragY = snd dragXY
        dx = physBub.dx + accelX + dragX
        dy = physBub.dy + accelY + dragY
        dampenedDxDy = dampen dx dy
    in
        { physBub | dx = fst dampenedDxDy, dy = snd dampenedDxDy }

-- A view of a PhysicsBubble, using an address at this level of the architecture

fwdingView : Context Action -> Phys.Model -> Svg
fwdingView context physModel =
    Phys.view (forwardTo context (Direct physModel.bubble.id)) physModel

view : Context Action -> Model -> List Svg
view context model =
    map (fwdingView context) model

