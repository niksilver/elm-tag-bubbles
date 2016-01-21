import Constants exposing (Id, Tag)
import UI
import MultiBubbles
import Bubble

import StartApp exposing (start)
import Dict exposing (Dict, empty, insert)
import Maybe exposing (Maybe(Nothing))
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal

width = 800

height = 600

multiBubbleModel =
    [ (Tag "us-news/us-news" "US News", 150)
    , (Tag "uk/uk" "UK", 100)
    , (Tag "society/doctors" "Doctors", 80)
    , (Tag "football/fa-cup" "FA Cup", 120)
    ]
        |> List.map (\p -> Bubble.makeBubble (fst p) (snd p))
        |> List.map Bubble.setToFadeIn
        |> MultiBubbles.arrangeCentre (width/2) (height/2)


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
    , status = "Welcome"
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

