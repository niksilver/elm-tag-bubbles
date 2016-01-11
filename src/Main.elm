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
        { id = "us-news/us-news"
        , x = 340, y = 200
        , size = 180, label = "US News" }
    }

phys2Model =
    {  dx = 0, dy = 0
    , bubble =
        { id = "uk/uk"
        , x = 300, y = 250
        , size = 100, label = "UK" }
    }

phys3Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "society/doctors"
        , x = 480, y = 350
        , size = 80, label = "Doctors" }
    }

phys4Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "football/fa-cup"
        , x = 400, y = 400
        , size = 120, label = "FA Cup" }
    }

springs : Dict (Id, Id) Float
springs =
    Dict.empty
        |> insert ("us-news/us-news", "uk/uk") 100
        |> insert ("uk/uk", "us-news/us-news") 100
        |> insert ("us-news/us-news", "society/doctors") 150
        |> insert ("society/doctors", "us-news/us-news") 150
        |> insert ("us-news/us-news", "football/fa-cup")  75
        |> insert ("football/fa-cup", "us-news/us-news")  75
        |> insert ("uk/uk", "society/doctors") 125
        |> insert ("society/doctors", "uk/uk") 125
        |> insert ("uk/uk", "football/fa-cup") 175
        |> insert ("football/fa-cup", "uk/uk") 175
        |> insert ("society/doctors", "football/fa-cup") 200
        |> insert ("football/fa-cup", "society/doctors") 200

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

