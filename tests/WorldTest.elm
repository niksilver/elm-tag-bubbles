module WorldTest (all) where

import ElmTest exposing (..)

import Constants exposing (navHeight, statusBarHeight, sideBorderWidth)
import World exposing (..)

all : Test
all =
    suite "WorldTest"
    [ sizeTest
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
