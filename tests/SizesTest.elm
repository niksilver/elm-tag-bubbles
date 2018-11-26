module SizesTest exposing (all)

import Sizes exposing (..)
import Constants exposing (Tag, Tags)

import Dict

import Test exposing (..)
import Expect exposing (..)


tag1rec = Tag "tag/1" "Tag one"

tag2rec = Tag "tag/2" "Tag two"

tag3rec = Tag "tag/3" "Tag three"

tag4rec = Tag "tag/4" "Tag four"

tag5rec = Tag "tag/5" "Tag five"

tag6rec = Tag "tag/6" "Tag six"

tagListA : List Tags
tagListA =
    [ [ tag1rec, tag2rec, tag3rec ]
    , [ tag1rec, tag2rec, tag4rec ]
    , [ tag1rec, tag5rec ]
    , [ tag5rec, tag6rec ]
    ]

all : Test
all =
    describe "SizesTest suite"
    [ toDictTest
    , rescaleTest
    , topNTest
    , filterTest
    ]

toDictTest : Test
toDictTest =
    describe "toDictTest"

    [ test "Empty list should yield empty dict" <|
      \_ ->
        Expect.equal
        0
        (toDict [] |> Dict.size)

    , test "Empty list of lists should yield empty dict" <|
      \_ ->
        Expect.equal
        0
        (toDict [[]] |> Dict.size)

    , test "Tag appearing once should have count of 1" <|
      \_ ->
        Expect.equal
        (Just 1)
        (toDict tagListA |> Dict.get "tag/6")

    , test "Tag appearing three times should have count of 3" <|
      \_ ->
        Expect.equal
        (Just 3)
        (toDict tagListA |> Dict.get "tag/1")

    , test "Tag appearing two times should have count of 2" <|
      \_ ->
        Expect.equal
        (Just 2)
        (toDict tagListA |> Dict.get "tag/2")

    ]

rescaleTest : Test
rescaleTest =
    describe "rescaleTest"

    [ test "Empty list should yield empty dict" <|
      \_ ->
        Expect.equal
        0
        (toDict [] |> rescale 11 22 |> Dict.size)

    , test "Empty list of lists should yield empty dict" <|
      \_ ->
        Expect.equal
        0
        (toDict [[]] |> rescale 11 22 |> Dict.size)

    , test "Tag appearing once should have min size" <|
      \_ ->
        Expect.equal
        (Just 11)
        (toDict tagListA |> rescale 11 22 |> Dict.get "tag/6")

    , test "Tag appearing most times should have max size" <|
      \_ ->
        Expect.equal
        (Just 22)
        (toDict tagListA |> rescale 11 22 |> Dict.get "tag/1")

    , test "Tag appearing middling times should have middling size" <|
      \_ ->
        Expect.equal
        (Just 16.5)
        (toDict tagListA |> rescale 11 22 |> Dict.get "tag/2")

    ]

topNTest : Test
topNTest =
    describe "topNTest"

    [ test "Top 3 of [] should be empty list" <|
      \_ ->
        Expect.equal
        []
        (topN 3 [])

    , test "Top 3 of [[]] should be empty list" <|
      \_ ->
        Expect.equal
        []
        (topN 3 [[]])

    , test "Top 1 of a normal collection should contain just the top one" <|
      \_ ->
        Expect.equal
        [tag1rec.id]
        (topN 1 tagListA)

    , test "Top 2 of a normal collection should contain just the top one" <|
      \_ ->
        Expect.equal
        [tag1rec.id, tag2rec.id]
        (topN 2 tagListA)

    , test "Top 4 of a normal collection should contain four items" <|
      \_ ->
        Expect.equal
        4
        (topN 4 tagListA |> List.length)

    , test "Top 10 of a collection with six tags should contain six items" <|
      \_ ->
        Expect.equal
        6
        (topN 10 tagListA |> List.length)

    ]

filterTest : Test
filterTest =
    describe "filterTest"

    [ test "Filtering on two tags should yield sensible result" <|
      \_ ->
        Expect.equal
        [ [ tag1rec, tag2rec ]
        , [ tag1rec, tag2rec ]
        , [ tag1rec ]
        ]
        (filter tagListA [ tag1rec.id, tag2rec.id ])

    , test "Filtering on just one tag should be okay" <|
      \_ ->
        Expect.equal
        [ [ tag2rec ]
        , [ tag2rec ]
        ]
        (filter tagListA [ tag2rec.id ])

    , test "Filtering on all tags should yield same as we started with" <|
      \_ ->
        Expect.equal
        tagListA
        (filter
          tagListA
          [ tag1rec.id, tag2rec.id, tag3rec.id, tag4rec.id, tag5rec.id, tag6rec.id])

    , test "Filtering on no tags should yield empty list" <|
      \_ ->
        Expect.equal
        []
        (filter tagListA [])

    ]



