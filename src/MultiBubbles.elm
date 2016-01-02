module MultiBubbles where

-- Multiple bubbles

import PhysicsBubble as Phys

import List exposing (reverse, length, indexedMap, map)
import Svg exposing (Svg)
import Signal exposing (Address, forwardTo)

type alias Model = List Phys.Model

type alias Id = String

type Action
    = Tick
    | Direct Id Phys.Action

update : Action -> Model -> Model
update action model =
    case action of
        Direct id physAct -> updateOne id physAct model
        Tick -> updateAll model

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

-- A view of a PhysicsBubble, using an address at this level of the architecture

fwdingView : Address Action -> Phys.Model -> Svg
fwdingView address physModel =
    Phys.view (forwardTo address (Direct physModel.bubble.id)) physModel

view : Address Action -> Model -> List Svg
view address model =
    map (fwdingView address) model

