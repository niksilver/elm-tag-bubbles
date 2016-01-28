import UI
import UI.Setup

import Html exposing (Html)
import Task exposing (Task)
import Signal exposing (Signal, foldp)
import Effects exposing (Effects, Never)
import Signal.Extra exposing (foldp')

-- Mailbox for tasks run by the containing system

taskBox : Signal.Mailbox (List UI.Action)
taskBox =
    Signal.mailbox [UI.NoOp]

singleton : a -> List a
singleton item =
    [ item ]

taskAddress : Signal.Address UI.Action
taskAddress =
    Signal.forwardTo taskBox.address singleton

-- A combination of external task results and our designated inputs

actionSignals : Signal (List UI.Action)
actionSignals =
    let
        singleInputs : Signal UI.Action
        singleInputs = Signal.mergeMany UI.inputs

        listedInputs : Signal (List UI.Action)
        listedInputs = Signal.map singleton singleInputs
    in
        Signal.merge listedInputs taskBox.signal

init : (UI.Model, Effects UI.Action)
init = (UI.Setup.model, UI.Setup.effects)

updates : Signal (UI.Model, Effects UI.Action)
updates =
    let
        updateOne : UI.Action -> (UI.Model, Effects UI.Action) -> (UI.Model, Effects UI.Action)
        updateOne action (model, _) =
            UI.update action model

        updateMany : List UI.Action -> (UI.Model, Effects UI.Action) -> (UI.Model, Effects UI.Action)
        updateMany actions modelAndEffect =
            List.foldl updateOne modelAndEffect actions

        initFn : List UI.Action -> (UI.Model, Effects UI.Action)
        initFn actions =
            updateMany actions init
    in
        foldp' updateMany initFn actionSignals

models : Signal UI.Model
models =
    Signal.map fst updates

main : Signal Html
main =
    Signal.map (UI.view taskAddress) models

port tasks : Signal (Task Never ())
port tasks =
    let
        taskify : Effects UI.Action -> Task Never ()
        taskify effect =
            Effects.toTask taskBox.address effect

        effects : Signal (Effects UI.Action)
        effects = Signal.map snd updates

    in
        Signal.map taskify effects

