module SpringsTest (all) where

import Springs exposing (..)
import Constants exposing (Tag, Tags)
import PairCounter exposing (countOf)

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

