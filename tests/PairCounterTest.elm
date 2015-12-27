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
      (assertEqual
        1
        (emptyCounter |> inc x y |> countOf x y))

    , test "Should be able to add to an empty counter twice and see what's added"
      (assertEqual
        2
        (emptyCounter |> inc x y |> inc x y |> countOf x y))
 
    , test "Should be able to add to an empty counter and other pairs are still zero"
      (assertEqual
        0
        (emptyCounter |> inc x y |> countOf x z))

    ]
