module UI.Setup exposing (init)


import Constants exposing
    ( Id, Tag
    , minBubbleSize, maxBubbleSize
    , minSpringLength, maxSpringLength
    )
import UI
import MultiBubbles
import World
import PairCounter exposing (emptyCounter, incPair)
import Springs
import Bubble
import Status exposing (Status)
import Help

import Browser.Dom as Dom
import Dict exposing (Dict, empty, insert)
import Task


-- Initial model

width = 800

height = 600

tags =
    [ Tag "uk/uk" "UK"
    , Tag "world/world" "World"
    , Tag "politics/politics" "Politics"
    , Tag "sport/sport" "Sport"
    , Tag "football/football" "Football"
    , Tag "culture/culture" "Culture"
    , Tag "business/business" "Business"
    , Tag "lifeandstyle/lifeandstyle" "Life and style"
    , Tag "environment/environment" "Environment"
    , Tag "technology/technology" "Technology"
    , Tag "travel/travel" "Travel"
    ]


size = minBubbleSize


multiBubbleModel : MultiBubbles.Model
multiBubbleModel =
    tags
        |> List.map (\bubble -> Bubble.make bubble size)
        |> List.map (Bubble.setOpacity 0.0)
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
    { dimensions = (width, height)
    , world =
        { bubbles = multiBubbleModel
        , springs = springs
        , dimensions = Nothing
        , scale = 1
        }
    , status = Status Nothing Nothing
    , help = Help.Off
    }


-- Create a resize message from a viewport

resizeMsg : Dom.Viewport -> UI.Msg
resizeMsg vp =
  UI.Resize (vp.viewport.width |> floor) (vp.viewport.height |> floor)


-- Initial state

init : () -> (UI.Model, Cmd UI.Msg)
init _ =
    ( model
    , Task.perform resizeMsg Dom.getViewport
    )

