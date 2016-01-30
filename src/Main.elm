import UI
import UI.Setup

import Constants exposing (TagsResult)

import Html exposing (Html)
import Task exposing (Task)
import Signal exposing (Signal, foldp)
import Signal.Extra exposing (foldp')

-- Mailbox for tasks run by the containing system

taskBox : Signal.Mailbox UI.Action
taskBox =
    Signal.mailbox UI.NoOp

-- A combination of external task results and our designated inputs

actionSignals : Signal UI.Action
actionSignals =
    let
        singleInputs : Signal UI.Action
        singleInputs = Signal.mergeMany UI.inputs
    in
        Signal.merge singleInputs taskBox.signal

init : (UI.Model, UI.TaskOut)
init = (UI.Setup.model, UI.Setup.task)

updates : Signal (UI.Model, UI.TaskOut)
updates =
    let
        updateOne : UI.Action -> (UI.Model, UI.TaskOut) -> (UI.Model, UI.TaskOut)
        updateOne action (model, _) =
            UI.update action model

        initFn : UI.Action -> (UI.Model, UI.TaskOut)
        initFn action =
            UI.update action (fst init)
    in
        foldp' updateOne initFn actionSignals

models : Signal UI.Model
models =
    Signal.map fst updates

main : Signal Html
main =
    Signal.map (UI.view taskBox.address) models

port tasks : Signal (Task () ())
port tasks =
    let
        send : Task () UI.Action -> Task () ()
        send t =
            Task.andThen t (Signal.send taskBox.address)
        get mTask =
            case mTask of
                Just task -> task
                Nothing -> Task.fail ()
    in
    Signal.map snd updates
        |> Signal.filter (\m -> m /= Nothing) Nothing
        |> Signal.map get
        |> Signal.map (Task.map UI.NewTags)
        |> Signal.map send

