module MultiBubblesTest exposing (all)

import Constants exposing (Tag)
import MultiBubbles exposing (..)
import Bubble

import Test exposing (..)
import Expect exposing (..)


tag1Rec = Tag "tag/one" "Tag one"
tag2Rec = Tag "tag/two" "Tag two"
tag3Rec = Tag "tag/three" "Tag three"
tag4Rec = Tag "tag/four" "Tag four"
tag5Rec = Tag "tag/five" "Tag five"


all : Test
all =
    describe "MultiBubbles suite"
    [ tagDiffTest
    , boundsTest
    ]


tagDiffTest : Test
tagDiffTest =
    describe "tagDiffTest"

    [ test "Diff from [] to some tags should say those tags are new" <|
      \_ ->
        Expect.equal
          (Diff [] [tag1Rec, tag2Rec] [])
          (tagDiff [] [tag1Rec, tag2Rec])

    , test "Diff from some tags to [] should say those tags are old" <|
      \_ ->
        Expect.equal
          (Diff [tag1Rec, tag2Rec] [] [])
          (tagDiff [tag1Rec, tag2Rec] [])

    , test "Diff with some overlapping tags should show old, new and both" <|
      \_ ->
        Expect.equal
          (Diff [tag1Rec, tag2Rec] [tag4Rec, tag5Rec] [tag3Rec])
          (tagDiff [tag1Rec, tag2Rec, tag3Rec] [tag3Rec, tag4Rec, tag5Rec])

    ]


bubble1 : Bubble.Model
bubble1 =
    { id = "bubble1"
    , x = 100, y = 120
    , dx = 0, dy = 0
    , size = 35
    , label = "Bubble One", animation = Bubble.noAnimation
    }


bubble2 : Bubble.Model
bubble2 =
    { id = "bubble2"
    , x = 150, y = 130
    , dx = 0, dy = 0
    , size = 40
    , label = "Bubble Two", animation = Bubble.noAnimation
    }

boundsTest : Test
boundsTest =
    describe "boundsTest"

    [ test "Bounds of no bubbles should be all zero" <|
      \_ ->
        Expect.equal
          (Bounds 0 0 0 0)
          (bounds [])

    , test "If just one bubble, bounds should be its bounds" <|
      \_ ->
        Expect.equal
          (Bounds (120 - 35) (100 + 35) (120 + 35) (100 - 35))
          (bounds [ bubble1 ])

    , test "If a second bubble right and down, bounds should be correct" <|
      \_ ->
        Expect.equal
          (Bounds (120 - 35) (150 + 40) (130 + 40) (100 - 35))
          (bounds [ bubble1, bubble2 ])

    , test "If a second bubble left and up, bounds should be correct" <|
      \_ ->
        Expect.equal
          (Bounds (120 - 35) (150 + 40) (130 + 40) (100 - 35))
          (bounds [ bubble2, bubble1 ])

    ]

