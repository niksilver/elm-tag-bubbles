import UI
import UI.Setup
import UI.Tasks

import Constants exposing (TagsResult)

import Html exposing (Html)
import Task exposing (Task)
import Signal exposing (Signal)

init : (UI.Model, UI.TaskOut)
init = (UI.Setup.model, UI.Setup.task)

updates : Signal (UI.Model, UI.TaskOut)
updates =
    UI.Tasks.updates init

models : Signal UI.Model
models =
    Signal.map fst updates

main : Signal Html
main =
    Signal.map (UI.view UI.Tasks.address) models

port tasks : Signal (Task () ())
port tasks =
    Signal.map snd updates
        |> UI.Tasks.tasks

