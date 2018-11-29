module WorldTest exposing (all)


import Test exposing (..)
import Expect exposing (..)

import Constants exposing (navHeight, statusBarHeight, sideBorderWidth)
import World exposing (..)


all : Test
all =
    describe "WorldTest suite"
    [ sizeTest
    , viewBoxTest
    , viewBoxToStringTest
    ]


expectCloseEnough : Float -> Float -> Expectation
expectCloseEnough =
  Expect.within (Relative 0.01)


sizeTest : Test
sizeTest =
    describe "sizeTest"

    [ test "Large window should yield world dimensions with min borders" <|
      \_ ->
        Expect.equal
          (1000 - 2 * sideBorderWidth, 800 - navHeight - statusBarHeight)
          (size (1000, 800))

    , test "Slightly-too-narrow window should yield reduced borders" <|
      \_ ->
        Expect.equal
          (Constants.minWorldWidth, 800 - navHeight - statusBarHeight)
          (size (Constants.minWorldWidth + 3 + 3, 800))

    , test "Very narrow window should yield world as wide as window" <|
      \_ ->
        Expect.equal
          (100, 800 - navHeight - statusBarHeight)
          (size (100, 800))

    ]


viewBoxTest : Test
viewBoxTest =
    describe "viewBoxTest"

    -- Testing minX...

    [ test "With a scale of 1, should give 0 for minX" <|
      \_ ->
        expectCloseEnough
          0
          (viewBox (201, 101) 1 |> .minX)

    , test "With a scale of 2, minX should be width/4" <|
      \_ ->
        expectCloseEnough
          50.25
          (viewBox (201, 101) 2 |> .minX)

    , test "With a scale of 3, minX should be width/3" <|
      \_ ->
        expectCloseEnough
          201
          (viewBox (603, 101) 3 |> .minX)

    -- Testing minY...

    , test "With a scale of 1, minY should be 0" <|
      \_ ->
        expectCloseEnough
          0
          (viewBox (201, 101) 1 |> .minY)

    , test "With a scale of 2, minY should be height/4" <|
      \_ ->
        expectCloseEnough
          22
          (viewBox (201, 88) 2 |> .minY)

    , test "With a scale of 3, minY should be height/3" <|
      \_ ->
        expectCloseEnough
          33
          (viewBox (201, 99) 3 |> .minY)

    -- Testing width...

    , test "With a scale of 1, width should be original width" <|
      \_ ->
        expectCloseEnough
          201
          (viewBox (201, 101) 1 |> .width)

    , test "With a scale of 2, width should be half original width" <|
      \_ ->
        expectCloseEnough
          100.5
          (viewBox (201, 88) 2 |> .width)

    -- Testing height...

    , test "With a scale of 1, height should be original height" <|
      \_ ->
        expectCloseEnough
          101
          (viewBox (201, 101) 1 |> .height)

    , test "With a scale of 2, height should be half original height" <|
      \_ ->
        expectCloseEnough
          44
          (viewBox (201, 88) 2 |> .height)

    ]


viewBoxToStringTest : Test
viewBoxToStringTest =
    describe "viewBoxToStringTest"

    [ test "A viewBox should convert to a string correctly" <|
      \_ ->
        Expect.equal
          "11 22 33 44"
          (ViewBox 11 22 33 44 |> viewBoxToString)

    , test "Another viewBox should convert to a string correctly" <|
      \_ ->
        Expect.equal
          "12 34 56 78"
          (ViewBox 12 34 56 78 |> viewBoxToString)

    ]
