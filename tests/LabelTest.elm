module LabelTest (all) where

import Label exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "LabelTest"
    [ pixelsToCharsTest
    , toPartsTest
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

toPartsTest : Test
toPartsTest =
    suite "toPartsTest"

    [ test "'C' should give C" <|
      assertEqual
      [ Block "C" ]
      (toParts "C")

    , test "'Cat' should give Cat" <|
      assertEqual
      [ Block "Cat" ]
      (toParts "Cat")

    , test "'Cat ' should give Cat/whitespace" <|
      assertEqual
      [ Block "Cat", Whitespace " " ]
      (toParts "Cat ")

    , test "'Cat five' should give Cat/whitespace/five" <|
      assertEqual
      [ Block "Cat", Whitespace " ", Block "five" ]
      (toParts "Cat five")

    , test "'Cat (dbl wspace) five' should give Cat/double-whitespace/five" <|
      assertEqual
      [ Block "Cat", Whitespace "  ", Block "five" ]
      (toParts "Cat  five")

    , test "' abc ' should give whitespace/abc/whitespace" <|
      assertEqual
      [ Whitespace " ", Block "abc", Whitespace " " ]
      (toParts " abc ")

    , test "'Cat (tab) five' should give Cat/whitespace/five" <|
      assertEqual
      [ Block "Cat", Whitespace "\t", Block "five" ]
      (toParts "Cat\tfive")

    ]

