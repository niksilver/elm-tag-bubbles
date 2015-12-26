import BubblesHtml exposing (update, view, Action(Tick))
import Constants exposing (colour1, colour2)

import StartApp exposing (start)
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal
import Time

bubble1Model =
    { x = 340, y = 200, dx = 0, dy = 1
    , size = 180, colour = colour2 }
bubble2Model =
    { x = 300, y = 250, dx = 1, dy = 0
    , size = 100, colour = colour1 }
bubble3Model =
    { x = 480, y = 350, dx = -1, dy = 0
    , size = 80, colour = colour1 }
bubble4Model =
    { x = 400, y = 400, dx = 0, dy = -1
    , size = 60, colour = colour2 }

model : BubblesHtml.Model
model = { width = 800
    , height = 600
    , bubbles =
        [bubble1Model
        , bubble2Model
        , bubble3Model
        , bubble4Model
        ]
    , newTags = [[]]
    }

ticker : Signal Action
ticker =
    Signal.map (always Tick) (Time.fps 30) 

app =
    start
        { init = (model, Effects.none)
        , update = update
        , view = view
        , inputs = [ticker]
        }

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

