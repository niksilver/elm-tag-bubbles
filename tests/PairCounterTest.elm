module PairCounterTest where

import PairCounter exposing (..)

import ElmTest exposing (..)

x = { id = "x" }
y = { id = "y" }
z = { id = "z" }

all : Test
all =
    suite "PairCounterTest"

    [ test "Empty counter should give zero for anything"
      (assertEqual
        0
        (countOf emptyCounter x y))

    , test "Should be able to add to an empty counter and see what's added"
      (let
          counter = inc emptyCounter x y
       in
          (assertEqual
            1
            (countOf counter x y)))

    , test "Should be able to add to an empty counter twice and see what's added"
      (let
          counter1 = inc emptyCounter x y
          counter2 = inc counter1 x y
       in
          (assertEqual
            2
            (countOf counter2 x y)))
 
    , test "Should be able to add to an empty counter and other pairs are still zero"
      (let
          counter = inc emptyCounter x y
       in
          (assertEqual
            0
            (countOf counter x z)))

    ]
