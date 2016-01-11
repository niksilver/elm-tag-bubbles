import Constants exposing (Id, colour1, colour2, emptyTagsResult)
import UI exposing
    ( initialEffects, update, view
    , Action(Tick, Click)
    , countedClicks
    )

import StartApp exposing (start)
import Dict exposing (Dict, empty, insert)
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal
import Time

phys1Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "b1"
        , x = 340, y = 200
        , size = 180, label = "One" }
    }

phys2Model =
    {  dx = 0, dy = 0
    , bubble =
        { id = "b2"
        , x = 300, y = 250
        , size = 100, label = "Two" }
    }

phys3Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "b3"
        , x = 480, y = 350
        , size = 80, label = "Three" }
    }

phys4Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "b4"
        , x = 400, y = 400
        , size = 120, label = "Four" }
    }

springs : Dict (Id, Id) Float
springs =
    Dict.empty
        |> insert ("b1", "b2") 100 |> insert ("b2", "b1") 100
        |> insert ("b1", "b3") 150 |> insert ("b3", "b1") 150
        |> insert ("b1", "b4")  75 |> insert ("b4", "b1")  75
        |> insert ("b2", "b3") 125 |> insert ("b3", "b2") 125
        |> insert ("b2", "b4") 175 |> insert ("b4", "b2") 175
        |> insert ("b3", "b4") 200 |> insert ("b4", "b3") 200

model : UI.Model
model =
    { width = 800
    , height = 600
    , world =
        { bubbles =
            [ phys1Model
            , phys2Model
            , phys3Model
            , phys4Model
            ]
        , springs = springs
        }
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
        , inputs = [ticker, countedClicks]
        }

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

