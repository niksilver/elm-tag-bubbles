module WorldTest (all) where

import ElmTest exposing (..)

import Constants exposing (navHeight, statusBarHeight, sideBorderWidth)
import World exposing (..)

all : Test
all =
    suite "WorldTest"
    [ sizeTest
    , viewBoxTest
    ]

sizeTest : Test
sizeTest =
    suite "sizeTest"

    [ test "Large window should yield world dimensions with min borders" <|
      assertEqual
        (1000 - 2 * sideBorderWidth, 800 - navHeight - statusBarHeight)
        (size (1000, 800))

    , test "Slightly-too-narrow window should yield reduced borders" <|
      assertEqual
        (Constants.minWorldWidth, 800 - navHeight - statusBarHeight)
        (size (Constants.minWorldWidth + 3 + 3, 800))

    , test "Very narrow window should yield world as wide as window" <|
      assertEqual
        (100, 800 - navHeight - statusBarHeight)
        (size (100, 800))

    ]

viewBoxTest : Test
viewBoxTest =
    suite "viewBoxTest"

    -- Testing minX...

    [ test "With a scale of 1, should give 0 for minX" <|
      assertEqual
        0
        (viewBox (201, 101) 1 |> .minX)

    , test "With a scale of 2, minX should be width/4" <|
      assertEqual
        50.25
        (viewBox (201, 101) 2 |> .minX)

    , test "With a scale of 3, minX should be width/3" <|
      assertEqual
        201
        (viewBox (603, 101) 3 |> .minX)

    -- Testing minY...

    , test "With a scale of 1, minY should be 0" <|
      assertEqual
        0
        (viewBox (201, 101) 1 |> .minY)

    , test "With a scale of 2, minY should be height/4" <|
      assertEqual
        22
        (viewBox (201, 88) 2 |> .minY)

    , test "With a scale of 3, minY should be height/3" <|
      assertEqual
        33
        (viewBox (201, 99) 3 |> .minY)

    -- Testing width...

    , test "With a scale of 1, width should be original width" <|
      assertEqual
        201
        (viewBox (201, 101) 1 |> .width)

    , test "With a scale of 2, width should be half original width" <|
      assertEqual
        100.5
        (viewBox (201, 88) 2 |> .width)

    -- Testing height...

    , test "With a scale of 1, height should be original height" <|
      assertEqual
        101
        (viewBox (201, 101) 1 |> .height)

    , test "With a scale of 2, height should be half original height" <|
      assertEqual
        44
        (viewBox (201, 88) 2 |> .height)

    ]
