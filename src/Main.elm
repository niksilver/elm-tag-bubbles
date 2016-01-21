import Constants exposing
    ( Id, Tag
    , minBubbleSize, maxBubbleSize
    , minSpringLength, maxSpringLength
    )
import UI
import MultiBubbles
import PairCounter exposing (emptyCounter, incPair)
import Springs
import Bubble

import StartApp exposing (start)
import Dict exposing (Dict, empty, insert)
import Maybe exposing (Maybe(Nothing))
import Task exposing (Task)
import Effects exposing (none, Never)
import Signal

width = 800

height = 600

tags =
    [ Tag "us-news/us-news" "US News"
    , Tag "uk/uk" "UK"
    , Tag "society/doctors" "Doctors"
    , Tag "football/fa-cup" "FA Cup"
    ]

size = (minBubbleSize + maxBubbleSize) / 2

multiBubbleModel : MultiBubbles.Model
multiBubbleModel =
    tags
        |> List.map (\bubble -> Bubble.makeBubble bubble size)
        |> List.map Bubble.setToFadeIn
        |> MultiBubbles.arrangeCentre (width/2) (height/2)

allPairs : List (Tag, Tag)
allPairs = PairCounter.allPairs tags

springs : Dict (Id, Id) Float
springs =
    List.foldl incPair emptyCounter allPairs
        |> Springs.toDictWithZeros minSpringLength maxSpringLength

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

