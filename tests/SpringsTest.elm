module SpringsTest (all) where

import Springs exposing (..)
import Constants exposing (Tag, Tags)
import PairCounter exposing (countOf, emptyCounter, set)

import Maybe exposing (Maybe(Just))
import Dict exposing (empty, insert)

import ElmTest exposing (..)

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

    [ test "Acceleration check of bubble1 x (too far right)" <|
      let
          bubble1 =
              { id = "b1"
              , x = 480, y = 350
              , size = 80, colour = "irrelevant" }
          bubble2 =
              { id = "b2"
              , x = 400, y = 400
              , size = 60, colour = "irrelevant" }
          stiffness = 20.0
          length = 60.0
          springs =
              empty
                  |> insert ("b1", "b2") length
                  |> insert ("b2", "b1") length
      in
          assertEqual
          (stiffness * (length-(480-400)) / (80 * 80))
          (acceleration stiffness springs bubble2 bubble1)

    ]
