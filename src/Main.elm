import UI
import UI.Setup exposing (init)
import UI.Tasks exposing (address)

import Constants exposing (TagsResult)

import Html exposing (Html)
import Task exposing (Task)
import Signal exposing (Signal)

updates : Signal (UI.Model, UI.TaskOut)
updates =
    UI.Tasks.updates init

models : Signal UI.Model
models =
    Signal.map fst updates

tasksOut : Signal UI.TaskOut
tasksOut =
    Signal.map snd updates

main : Signal Html
main =
    Signal.map (UI.view address) models

port tasks : Signal (Task () ())
port tasks =
    UI.Tasks.tasks tasksOut

