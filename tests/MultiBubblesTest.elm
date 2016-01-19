module MultiBubblesTest (all) where

import Constants exposing (Tag)
import MultiBubbles exposing (..)

import ElmTest exposing (..)

tag1Rec = Tag "tag/one" "Tag one"
tag2Rec = Tag "tag/two" "Tag two"
tag3Rec = Tag "tag/three" "Tag three"
tag4Rec = Tag "tag/four" "Tag four"
tag5Rec = Tag "tag/five" "Tag five"

all : Test
all =
    suite "MultiBubbles"
    [ tagDiffTest
    ]

tagDiffTest : Test
tagDiffTest =
    suite "tagDiffTest"

    [ test "Diff from [] to some tags should say those tags are new" <|
      assertEqual
        (Diff [] [tag1Rec, tag2Rec] [])
        (tagDiff [] [tag1Rec, tag2Rec])

    , test "Diff from some tags to [] should say those tags are old" <|
      assertEqual
        (Diff [tag1Rec, tag2Rec] [] [])
        (tagDiff [tag1Rec, tag2Rec] [])

    , test "Diff with some overlapping tags should show old, new and both" <|
      assertEqual
        (Diff [tag1Rec, tag2Rec] [tag4Rec, tag5Rec] [tag3Rec])
        (tagDiff [tag1Rec, tag2Rec, tag3Rec] [tag3Rec, tag4Rec, tag5Rec])

    ]
