import BubblesHtml exposing (update, view)

import StartApp exposing (start)
import Task exposing (Task)
import Effects exposing (none, Never)

bubble1Model =
    { x = 340, y = 200, dx = 0, dy = 1
    , size = 180, colour = "red" }
bubble2Model =
    { x = 450, y = 250, dx = 0, dy = 1
    , size = 100, colour = "green" }
bubble3Model =
    { x = 480, y = 350, dx = -1, dy = 0
    , size = 80, colour = "red" }
bubble4Model =
    { x = 400, y = 400, dx = 1, dy = 0
    , size = 60, colour = "green" }

model = { width = 800
    , height = 600
    , bubbles =
        [bubble1Model
        , bubble2Model
        , bubble3Model
        , bubble4Model
        ]
    }

app =
    start
        { init = (model, Effects.none)
        , update = update
        , view = view
        , inputs = []
        }

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

