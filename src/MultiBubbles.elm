module MultiBubbles where

-- Multiple bubbles

import PhysicsBubble as Phys

import List exposing (reverse, length, indexedMap, map)
import Svg exposing (Svg)
import Signal exposing (Address, forwardTo)

type alias Model = List Phys.Model

type alias SingleAction = { id : String, act : Phys.Action }

type Action =
    Tick | One SingleAction

update : Action -> Model -> Model
update action model =
    case action of
        One act -> updateOne act model
        Tick -> updateAll model

updateOne : SingleAction -> Model -> Model
updateOne act model =
    map (selectiveUpdate act) model

selectiveUpdate : SingleAction -> Phys.Model -> Phys.Model
selectiveUpdate action physModel =
    if action.id == physModel.bubble.id then
        (Phys.update action.act physModel)
    else
        physModel

-- Update all the bubbles with the Tick action

updateAll : Model -> Model
updateAll model =
    map (\mod -> Phys.update Phys.Move mod) model

-- Create a forwarding address for the PhysicsBubble.view which will enable
-- its actions to be sent to and interpretted by this update mechanism.

fwdingAddress : Address Action -> String -> Address Phys.Action
fwdingAddress address id =
    forwardTo address (\a -> One { id = id, act = a })

-- A view of a PhysicsBubble, using an address at this level of the architecture

fwdingView : Address Action -> Phys.Model -> Svg
fwdingView address physModel =
    Phys.view (fwdingAddress address physModel.bubble.id) physModel

view : Address Action -> Model -> List Svg
view address model =
    map (fwdingView address) model

