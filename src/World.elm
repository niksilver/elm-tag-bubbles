module World where

import Constants exposing (Id)
import MultiBubbles as MB

import Dict exposing (Dict)
import Signal exposing (forwardTo)
import Svg exposing (Svg)


type alias Model =
    { bubbles : MB.Model
    , springs : Dict (Id,Id) Float
    }

type Action
    = Direct MB.Action
    | Tick

update : Action -> Model -> Model
update action model =
    case action of
        Direct act -> { model | bubbles = MB.update act model.bubbles }
        Tick -> { model | bubbles = MB.update MB.Tick model.bubbles }

view : Signal.Address Action -> Model -> List Svg
view address model =
    MB.view (forwardTo address Direct) model.bubbles

