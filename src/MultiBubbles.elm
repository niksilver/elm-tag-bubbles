module MultiBubbles where

-- Multiple bubbles

import PhysicsBubble as Phys

import List exposing (reverse, length, indexedMap, map)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Svg exposing (Svg)
import Signal exposing (Address, forwardTo)

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
        v = sqrt (physBub.dx^2 + physBub.dy^2)
        drag = -1.0 * v * v
        dragX = if v == 0 then 0 else drag * physBub.dx / v
        dragY = if v == 0 then 0 else drag * physBub.dy / v
        dragX' = if abs dragX > abs physBub.dx then -physBub.dx else dragX
        dragY' = if abs dragY > abs physBub.dy then -physBub.dy else dragY
        dx = physBub.dx + accelX + dragX
        dy = physBub.dy + accelY + dragY
    in
        { physBub | dx = dx, dy = dy }

-- A view of a PhysicsBubble, using an address at this level of the architecture

fwdingView : Address Action -> Phys.Model -> Svg
fwdingView address physModel =
    Phys.view (forwardTo address (Direct physModel.bubble.id)) physModel

view : Address Action -> Model -> List Svg
view address model =
    map (fwdingView address) model

