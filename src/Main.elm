import Constants exposing (colour1, colour2, emptyTagsResult)
import UI exposing (initialEffects, update, view, Action(Tick))

import StartApp exposing (start)
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal
import Time

phys1Model =
    { dx = 0, dy = 1
    , bubble =
        { id = "b1"
        , x = 340, y = 200
        , size = 180, colour = colour2 }
    }

phys2Model =
    {  dx = 1, dy = 0
    , bubble =
        { id = "b2"
        , x = 300, y = 250
        , size = 100, colour = colour1 }
    }

phys3Model =
    { dx = -1, dy = 0
    , bubble =
        { id = "b3"
        , x = 480, y = 350
        , size = 80, colour = colour1 }
    }

phys4Model =
    { dx = 0, dy = -1
    , bubble =
        { id = "b4"
        , x = 400, y = 400
        , size = 60, colour = colour2 }
    }

model : UI.Model
model = { width = 800
    , height = 600
    , bubbles =
        [ phys1Model
        , phys2Model
        , phys3Model
        , phys4Model
        ]
    , newTags = emptyTagsResult
    }

ticker : Signal Action
ticker =
    Signal.map (always Tick) (Time.fps 30) 

app =
    start
        { init = (model, initialEffects)
        , update = update
        , view = view
        , inputs = [ticker]
        }

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

