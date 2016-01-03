module World where

import Constants exposing (Id)
import MultiBubbles as MB
import Springs exposing (acceleration, accelDict)

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
        Tick ->
            let
                strength = 20.0
                accelFn = acceleration strength model.springs
                bubbles = List.map .bubble model.bubbles
                accels = accelDict bubbles accelFn
                bubsMod = MB.update (MB.AdjustVelocities accels) model.bubbles
                model' = { model | bubbles = bubsMod }
            in
                update (Direct MB.Tick) model'

view : Signal.Address Action -> Model -> List Svg
view address model =
    MB.view (forwardTo address Direct) model.bubbles

