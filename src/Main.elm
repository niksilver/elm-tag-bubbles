import Constants exposing (Id, emptyTagsResult)
import UI
import MultiBubbles
import Bubble exposing (Fading(None))

import StartApp exposing (start)
import Dict exposing (Dict, empty, insert)
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal

width = 800

height = 600

phys1Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "us-news/us-news"
        , x = 340, y = 200
        , size = 180, label = "US News"
        , fading = None, opacity = 0.0
        }
    }

phys2Model =
    {  dx = 0, dy = 0
    , bubble =
        { id = "uk/uk"
        , x = 300, y = 250
        , size = 100, label = "UK"
        , fading = None, opacity = 0.0
        }
    }

phys3Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "society/doctors"
        , x = 480, y = 350
        , size = 80, label = "Doctors"
        , fading = None, opacity = 0.0
        }
    }

phys4Model =
    { dx = 0, dy = 0
    , bubble =
        { id = "football/fa-cup"
        , x = 400, y = 400
        , size = 120, label = "FA Cup"
        , fading = None, opacity = 0.0
        }
    }

multiBubbleModel =
    MultiBubbles.initialModel
        width
        height
        [ phys1Model
        , phys2Model
        , phys3Model
        , phys4Model
        ]

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
    { width = width
    , height = height
    , world =
        { bubbles = multiBubbleModel
        , springs = springs
        }
    , newTags = emptyTagsResult
    }

app =
    start
        { init = (model, UI.initialEffects)
        , update = UI.update
        , view = UI.view
        , inputs = UI.initialInputs
        }

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

