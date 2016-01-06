module World where

import Constants exposing (Id, springStrength)
import Context exposing (Context, forwardTo)
import MultiBubbles as MB
import Springs exposing (acceleration, accelDict)

import Dict exposing (Dict)
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
                strength = springStrength
                accelFn = acceleration strength model.springs
                bubbles = List.map .bubble model.bubbles
                accels = accelDict bubbles accelFn
                bubsMod = MB.update (MB.AdjustVelocities accels) model.bubbles
                model' = { model | bubbles = bubsMod }
            in
                update (Direct MB.Tick) model'

view : Context Action -> Model -> List Svg
view context model =
    MB.view (forwardTo context Direct) model.bubbles

