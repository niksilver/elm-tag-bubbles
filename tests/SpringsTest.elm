module SpringsTest (all) where

import Springs exposing (..)
import Constants exposing (Tag, Tags)
import PairCounter exposing (countOf, emptyCounter, set)
import Bubble exposing (Fading(In))

import Maybe exposing (Maybe(Just))
import Dict exposing (empty, insert)

import ElmTest exposing (..)
import ElmTestExtras exposing (..)

tag1rec = Tag "world/bali-nine" "Bali Nine" "world"

tag2rec = Tag "australia-news/q-a" "Q&amp;A" "australia-news"

tag3rec = Tag "environment/cecil-the-lion" "Cecil the lion" "environment"

tag4rec = Tag "world/zimbabwe" "Zimbabwe" "world"

tag5rec = Tag "lifeandstyle/shops-and-shopping" "Shops and shopping" "lifeandstyle"

tag6rec = Tag "money/consumer-affairs" "Consumer affairs" "money"

tagListA : List Tags
tagListA =
    [ [ tag1rec, tag2rec, tag3rec ]
    , [ tag1rec, tag2rec, tag4rec ]
    , [ tag5rec, tag6rec ]
    ]

all : Test
all =
    suite "SpringsTest"
    [ counterTest
    , lengthsTest
    , accelerationTest
    , accelDictTest
    ]

counterTest : Test
counterTest =
    suite "counter"

    [ test "Two tags together twice should have a count of 2" <|
      assertEqual
      2
      (counter tagListA |> countOf tag1rec tag2rec)

    , test "Tags together twice should have a count of 2 the other way round" <|
      assertEqual
      2
      (counter tagListA |> countOf tag2rec tag1rec)

    , test "Tags not together should have a count of 0" <|
      assertEqual
      0
      (counter tagListA |> countOf tag3rec tag4rec)

    , test "Tags together once should have a count of 1" <|
      assertEqual
      1
      (counter tagListA |> countOf tag5rec tag6rec)

    ]

lengthsTest : Test
lengthsTest =
    suite "lengthsTest"

    [ test "Pair with highest count should have the shortest length" <|
      assertEqual
      (Just 33.3)
      (emptyCounter
        |> set tag1rec tag2rec 10
        |> set tag1rec tag3rec 3
        |> lengths 33.3 99.9
        |> Dict.get (tag1rec.id, tag2rec.id))

    , test "Pair with highest count should have the shortest length when reveresed" <|
      assertEqual
      (Just 33.3)
      (emptyCounter
        |> set tag1rec tag2rec 10
        |> set tag1rec tag3rec 3
        |> lengths 33.3 99.9
        |> Dict.get (tag2rec.id, tag1rec.id))

    , test "Pair with lowest count should have the longest length" <|
      assertEqual
      (Just 99.9)
      (emptyCounter
        |> set tag1rec tag2rec 10
        |> set tag1rec tag3rec 3
        |> lengths 33.3 99.9
        |> Dict.get (tag1rec.id, tag3rec.id))

    , test "Pair with middling count should have proportional length" <|
      assertEqual
      (Just (99 - 1 / 6 * 66))
      (emptyCounter
        |> set tag1rec tag2rec 10
        |> set tag1rec tag3rec 4
        |> set tag2rec tag3rec 5   -- This count is 1/6 of the count range
        |> lengths 33 99
        |> Dict.get (tag2rec.id, tag3rec.id))

    ]

accelerationTest : Test
accelerationTest =
    suite "accelerationTest"
    -- acceleration = (stiffness x distance) / mass
    -- mass is size squared

    [ test "Acceleration of bubble1 x-axis (too far right, stretched spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 60.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 480 - 400
          bubbleDistance = sqrt (80^2 + 50^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          (stiffness * -springXExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> fst)

    , test "Acceleration of bubble1 x-axis (too far left, compressed spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 480 - 400
          bubbleDistance = sqrt (80^2 + 50^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          (stiffness * -springXExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> fst)

    , test "Acceleration of bubble2 x-axis (too far left, stretched spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 60.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 400 - 480
          bubbleDistance = sqrt (80^2 + 50^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          (stiffness * -springXExtension / (60 * 60))
          (acceleration stiffness springs bubble1 bubble2 |> fst)

    , test "Acceleration of bubble2 x-axis (too far right, compressed spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 400 - 480
          bubbleDistance = sqrt (80^2 + 50^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          -- bubbleXDistance = -80
          -- springXLength = -100
          -- springXExtension = 20
          (stiffness * -springXExtension / (60 * 60))
          (acceleration stiffness springs bubble1 bubble2 |> fst)

    , test "Acceleration of bubble1 x-axis when bubble2 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 480, y = 350
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 479 - 480
          bubbleDistance = sqrt (1^2 + 1^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          (stiffness * -springXExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> fst)

    , test "Acceleration of bubble1 y-axis when bubble2 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 480, y = 350
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleYDistance = 349 - 350
          bubbleDistance = sqrt (1^2 + 1^2)
          springYLength = bubbleYDistance / bubbleDistance * springLength
          springYExtension = bubbleYDistance - springYLength
      in
          assertEqualTo4DP
          (stiffness * -springYExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> snd)

    , test "Acceleration of bubble2 x-axis when bubble1 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 480, y = 350
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleXDistance = 480 - 479
          bubbleDistance = sqrt (1^2 + 1^2)
          springXLength = bubbleXDistance / bubbleDistance * springLength
          springXExtension = bubbleXDistance - springXLength
      in
          assertEqualTo4DP
          (stiffness * -springXExtension / (60 * 60))
          (acceleration stiffness springs bubble1 bubble2 |> fst)

    , test "Acceleration of bubble2 y-axis when bubble1 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 480, y = 350
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleYDistance = 350 - 349
          bubbleDistance = sqrt (1^2 + 1^2)
          springYLength = bubbleYDistance / bubbleDistance * springLength
          springYExtension = bubbleYDistance - springYLength
      in
          assertEqualTo4DP
          (stiffness * -springYExtension / (60 * 60))
          (acceleration stiffness springs bubble1 bubble2 |> snd)

    , test "Acceleration of bubble1 y-axis (too far up, stretched spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 60.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleYDistance = 350 - 400
          bubbleDistance = sqrt (80^2 + 50^2)
          springYLength = bubbleYDistance / bubbleDistance * springLength
          springYExtension = bubbleYDistance - springYLength
      in
          assertEqualTo4DP
          -- bubbleYDistance = -50
          -- springYLength = -30
          -- springYExtension = -20
          (stiffness * -springYExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> snd)

    , test "Acceleration of bubble1 y-axis (too far down, compressed spring)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs =
              empty
                  |> insert ("b1", "b2") springLength
                  |> insert ("b2", "b1") springLength
          bubbleYDistance = 350 - 400
          bubbleDistance = sqrt (80^2 + 50^2)
          springYLength = bubbleYDistance / bubbleDistance * springLength
          springYExtension = bubbleYDistance - springYLength
      in
          assertEqualTo4DP
          -- bubbleYDistance = -50
          -- springYLength = -63
          -- springYExtension = 13
          (stiffness * -springYExtension / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1 |> snd)

    , test "Acceleration of bubbles not linked should give zero acceleration" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, label = "Bubble 1"
              , fading = In, opacity = 0.0
              }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, label = "Bubble 2"
              , fading = In, opacity = 0.0
              }
          stiffness = 20.0
          springLength = 120.0
          springs = empty
      in
          assertEqual
          (0, 0)
          (acceleration stiffness springs bubble2 bubble1)

    ]

accelDictTest : Test
accelDictTest =
    suite "accelDictTest"

    [ test ("Bubble1 pulled down equally, but never across, should have " ++
        "acceleration (3, 0)") <|
      let
          bubbleX =
              { id = "n/a", x = 0, y = 0 , size = 80, label = "X"
              , fading = In, opacity = 0.0 }
          bubble1 = { bubbleX | id = "b1", label = "B1" }
          bubble2 = { bubbleX | id = "b2", label = "B2" }
          bubble3 = { bubbleX | id = "b3", label = "B3" }
          bubble4 = { bubbleX | id = "b4", label = "B4" }
          bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
          accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
          accelFun bubA bubB =
              case (bubA.id, bubB.id) of
                  ("b1", "b1") -> (0, 0)
                  ("b2", "b1") -> (1, 0)
                  ("b3", "b1") -> (1, 0)
                  ("b4", "b1") -> (1, 0)
                  _ -> (99, 99)
      in
          assertEqual
          (Just (3, 0))
          (accelDict bubbles accelFun |> Dict.get "b1")

    , test ("Bubble1 pulled down unequally, but never across, should have " ++
        "acceleration (6, 0)") <|
      let
          bubbleX =
              { id = "n/a", x = 0, y = 0 , size = 80, label = "X"
              , fading = In, opacity = 0.0 }
          bubble1 = { bubbleX | id = "b1", label = "B1" }
          bubble2 = { bubbleX | id = "b2", label = "B2" }
          bubble3 = { bubbleX | id = "b3", label = "B3" }
          bubble4 = { bubbleX | id = "b4", label = "B4" }
          bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
          accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
          accelFun bubA bubB =
              case (bubA.id, bubB.id) of
                  ("b1", "b1") -> (0, 0)
                  ("b2", "b1") -> (1, 0)
                  ("b3", "b1") -> (2, 0)
                  ("b4", "b1") -> (3, 0)
                  _ -> (99, 99)
      in
          assertEqual
          (Just (6, 0))
          (accelDict bubbles accelFun |> Dict.get "b1")

    , test "Bubble2 pulled in different directions" <|
      let
          bubbleX =
              { id = "n/a", x = 0, y = 0 , size = 80, label = "X"
              , fading = In, opacity = 0.0 }
          bubble1 = { bubbleX | id = "b1", label = "B1" }
          bubble2 = { bubbleX | id = "b2", label = "B2" }
          bubble3 = { bubbleX | id = "b3", label = "B3" }
          bubble4 = { bubbleX | id = "b4", label = "B4" }
          bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
          accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
          accelFun bubA bubB =
              case (bubA.id, bubB.id) of
                  ("b1", "b2") -> (-10, 5)
                  ("b2", "b2") -> (0, 0)
                  ("b3", "b2") -> (1, 30)
                  ("b4", "b2") -> (2, -2)
                  _ -> (99, 99)
      in
          assertEqual
          (Just (-7, 33))
          (accelDict bubbles accelFun |> Dict.get "b2")

    ]
