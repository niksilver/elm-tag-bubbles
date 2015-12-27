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

{-
-- Update only the ith element of the model.
-- Do so by recursively going through the model building up an
-- accumulated result. Each time round the loop we see if we've
-- got the element we need to update.

updateOne : SingleAction -> Model -> Model -> Model
updateOne action model accum =
    if (action.idx == length accum) then
        case model of
            hd :: tl ->
                updateOne action tl (Bubble.update action.act hd :: accum)
            [] ->
                reverse accum
    else
        case model of
            hd :: tl ->
                updateOne action tl (hd :: accum)
            [] ->
                reverse accum

-}

-- Update all the bubbles with the Tick action

updateAll : Model -> Model
updateAll model =
    map (\mod -> Bubble.update Bubble.Move mod) model

{-
liftAction : Int -> Bubble.Action -> Action
liftAction i action =
    case action of
        Bubble.Flip -> One { idx = i, act = Bubble.Flip }
        Bubble.Move -> Tick
-}

fwdingView : Address Action -> Bubble.Model -> Svg
fwdingView address bubModel =
    Bubble.view (forwardTo address (\a -> One { id = bubModel.id, act = a })) bubModel


view : Address Action -> Model -> List Svg
view address model =
    map (fwdingView address) model

