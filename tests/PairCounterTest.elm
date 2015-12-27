module PairCounterTest where

import PairCounter exposing (..)

import ElmTest exposing (..)

x = { id = "x" }
y = { id = "y" }

all : Test
all =
    suite "PairCounterTest"

    [ test "Empty counter should give zero for anything"
      (assertEqual
        0
        (countOf emptyCounter x y))

    , test "Should be able to add to an empty counter and see what's added"
      (let
          c = inc emptyCounter x y
       in
          (assertEqual
            1
            (countOf c x y)))
    ]
