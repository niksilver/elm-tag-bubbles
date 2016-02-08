module LabelTest (all) where

import Label exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "LabelTest"
    [
        pixelsToCharsTest
    ]

pixelsToCharsTest : Test
pixelsToCharsTest =
    suite "pixelsToCharsTest"

    [ test "16 pixels should be 1 char (and a bit more, but it's rounded)" <|
      assertEqual
      1
      (pixelsToChars 16)

    , test "32 pixels should be 3 chars from measuring it on the screen" <|
      assertEqual
      3
      (pixelsToChars 32)

    , test "80 pixels should be 9 chars, from measuring it on the screen" <|
      assertEqual
      9
      (pixelsToChars 80)

    ]

