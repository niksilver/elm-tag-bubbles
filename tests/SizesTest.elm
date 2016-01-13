module SizesTest (all) where

import Sizes exposing (..)
import Constants exposing (Tag, Tags)

import Dict

import ElmTest exposing (..)

tag1rec = Tag "tag/1" "Tag one" "tag-one"

tag2rec = Tag "tag/2" "Tag two" "tag-two"

tag3rec = Tag "tag/3" "Tag three" "tag-three"

tag4rec = Tag "tag/4" "Tag four" "tag-four"

tag5rec = Tag "tag/5" "Tag five" "tag-five"

tag6rec = Tag "tag/6" "Tag six" "tag-six"

tagListA : List Tags
tagListA =
    [ [ tag1rec, tag2rec, tag3rec ]
    , [ tag1rec, tag2rec, tag4rec ]
    , [ tag1rec, tag5rec ]
    , [ tag5rec, tag6rec ]
    ]

all : Test
all =
    suite "SizesTest"
    [ toDictTest
    ]

toDictTest : Test
toDictTest =
    suite "toDictTest"

    [ test "Empty list should yield empty dict" <|
      assertEqual
      0
      (toDict 11 22 [] |> Dict.size)

    , test "Empty list of lists should yield empty dict" <|
      assertEqual
      0
      (toDict 11 22 [[]] |> Dict.size)

    , test "Tag appearing once should have min size" <|
      assertEqual
      (Just 11)
      (toDict 11 22 tagListA |> Dict.get "tag/6")

    , test "Tag appearing most times should have max size" <|
      assertEqual
      (Just 22)
      (toDict 11 22 tagListA |> Dict.get "tag/1")

    , test "Tag appearing middling times should have middling size" <|
      assertEqual
      (Just 16.5)
      (toDict 11 22 tagListA |> Dict.get "tag/2")

    ]

