module MultiBubbles where

-- Multiple bubbles

import Bubble

import List exposing (reverse, length, indexedMap, map)
import Svg exposing (Svg)
import Signal exposing (Address, forwardTo)

type alias Model = List Bubble.Model

type alias SingleAction = { id : String, act : Bubble.Action }

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

selectiveUpdate : SingleAction -> Bubble.Model -> Bubble.Model
selectiveUpdate action bubModel =
    if action.id == bubModel.id then
        (Bubble.update action.act bubModel)
    else
        bubModel

-- Update all the bubbles with the Tick action

updateAll : Model -> Model
updateAll model =
    map (\mod -> Bubble.update Bubble.Move mod) model

-- Create a forwarding address for the Bubble.view which will enable
-- its actions to be sent to and interpretted by this update mechanism.

fwdingAddress : Address Action -> String -> Address Bubble.Action
fwdingAddress address id =
    forwardTo address (\a -> One { id = id, act = a })

-- A view of a Bubble, using an address at this level of the architecture

fwdingView : Address Action -> Bubble.Model -> Svg
fwdingView address bubModel =
    Bubble.view (fwdingAddress address bubModel.id) bubModel

view : Address Action -> Model -> List Svg
view address model =
    map (fwdingView address) model

