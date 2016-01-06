module MultiBubbles where

-- Multiple bubbles

import Context exposing (Context, forwardTo)
import PhysicsBubble as Phys
import Springs exposing (drag, dampen)

import List exposing (reverse, length, indexedMap, map)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Svg exposing (Svg)

type alias Model = List Phys.Model

type alias Id = String

type Action
    = Tick
    | Direct Id Phys.Action
    | AdjustVelocities (Dict Id (Float, Float))

update : Action -> Model -> Model
update action model =
    case action of
        Direct id physAct -> updateOne id physAct model
        Tick -> updateAll model
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

updateAll : Model -> Model
updateAll model =
    map (Phys.update Phys.Move) model

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

