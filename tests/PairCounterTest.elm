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
        (countOf x y emptyCounter))

    , test "Should be able to add to an empty counter and see what's added"
      (let
          counter = inc x y emptyCounter
       in
          (assertEqual
            1
            (countOf x y counter)))

    , test "Should be able to add to an empty counter twice and see what's added"
      (let
          counter1 = inc x y emptyCounter
          counter2 = inc x y counter1
       in
          (assertEqual
            2
            (countOf x y counter2)))
 
    , test "Should be able to add to an empty counter and other pairs are still zero"
      (let
          counter = inc x y emptyCounter
       in
          (assertEqual
            0
            (countOf x z counter)))

    ]
