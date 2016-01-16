module World
    ( Model, Action
    , update, view
    , Diff, tagDiff
    ) where

import Constants exposing (Id, Tags, springStrength)
import Context exposing (Context, forwardTo)
import MultiBubbles as MB
import Springs exposing (acceleration, accelDict)

import Dict exposing (Dict)
import Svg exposing (Svg)
import Time exposing (Time)

type alias Model =
    { bubbles : MB.Model
    , springs : Dict (Id,Id) Float
    }

type Action
    = Direct MB.Action
    | Tick Time
    | NewTags (List Tags)

-- The difference between tags already in the world and tags fetched
-- via the API.

type alias Diff = { old : Tags, new : Tags, both : Tags }

update : Action -> Model -> Model
update action model =
    case action of
        Direct act -> { model | bubbles = MB.update act model.bubbles }
        Tick time ->
            let
                strength = springStrength
                accelFn = acceleration strength model.springs
                bubbles = List.map .bubble model.bubbles
                accels = accelDict bubbles accelFn
                bubsMod = MB.update (MB.AdjustVelocities accels) model.bubbles
                model' = { model | bubbles = bubsMod }
            in
                update (Direct (MB.Tick time)) model'
        NewTags listListTag ->
            model

view : Context Action -> Model -> List Svg
view context model =
    MB.view (forwardTo context Direct) model.bubbles

-- Find the difference between tags represented in the world
-- and tags fetched by the API

tagDiff : Tags -> Tags -> Diff
tagDiff current latest =
    let
        old = List.filter (\e -> not(List.member e latest)) current
        new = List.filter (\e -> not(List.member e current)) latest
        both = List.filter (\e -> List.member e current) latest
    in
        Diff old new both

