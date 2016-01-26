import UI
import UI.Setup

import StartApp exposing (start)
import Task exposing (Task)
import Signal
import Effects exposing (Never)

app =
    start
        { init = (UI.Setup.model, UI.Setup.effects)
        , update = UI.update
        , view = UI.view
        , inputs = UI.inputs
        }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

