module MultiBubbles where

-- Multiple bubbles

import Bubble

import List exposing (reverse, length, indexedMap)
import Svg exposing (Svg)
import Signal exposing (Address, forwardTo)

type alias Model = List Bubble.Model

type alias Action = { idx : Int, act : Bubble.Action }

update : Action -> Model -> Model
update action model =
    update' action model []

-- Update only the ith element of the model.
-- Do so by recursively going through the model building up an
-- accumulated result. Each time round the loop we see if we've
-- got the element we need to update.

update' : Action -> Model -> Model -> Model
update' action model accum =
    if (action.idx == length accum) then
        case model of
            hd :: tl -> update' action tl (Bubble.update action.act hd :: accum)
            [] -> reverse accum
    else
        case model of
            hd :: tl -> update' action tl (hd :: accum)
            [] -> reverse accum

fwdingView : Address Action -> Int -> Bubble.Model -> Svg
fwdingView address i bub =
    Bubble.view (forwardTo address (\act -> { idx = i, act = act})) bub

view : Address Action -> Model -> List Svg
view address model =
    indexedMap (fwdingView address) model

