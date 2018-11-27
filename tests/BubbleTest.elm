module BubbleTest exposing (all)

import Bubble exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "BubbleTest suite"
    [ linearEaseTest
    ]


expectCloseEnough : Float -> Float -> Expectation
expectCloseEnough =
  Expect.within (Relative 0.01)


linearEaseTest : Test
linearEaseTest =
    describe "linearEaseTest"

    [ test "Mid forward: Out range 5-10, input 0-100, if we're at 50 we should output 7.5" <|
      \_ ->
        expectCloseEnough
        7.5
        (linearEase 5 10 100 50)

    , test "Mid backward: Out range 10-5, input 0-100, if we're at 50 we should output 7.5" <|
      \_ ->
        expectCloseEnough
        7.5
        (linearEase 10 5 100 50)

    , test "Start forward: Out range 2-21, input 0-30, if we're at 0 we should output 2" <|
      \_ ->
        expectCloseEnough
        2.0
        (linearEase 2 21 30 0)

    , test "Start, backward: Out range 21-2, input 0-30, if we're at 0 we should output 21" <|
      \_ ->
        expectCloseEnough
        21.0
        (linearEase 21 2 30 0)

    , test "End forward: Out range 100-101, input 0-1, if we're at 1 we should output 101" <|
      \_ ->
        expectCloseEnough
        101.0
        (linearEase 100 101 1 1)

    , test "End backward: Out range 101-100, input 0-1, if we're at 1 we should output 100" <|
      \_ ->
        expectCloseEnough
        100.0
        (linearEase 101 100 1 1)

    ]


