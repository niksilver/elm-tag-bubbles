module LabelTest (all) where

import Label exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "LabelTest"
    [ pixelsToCharsTest
    , splitTest
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

half1 : Split -> String
half1 split =
    case split of
        Halves (a, b) -> a
        Whole _ -> "Error - got a whole looking for half1"

splitTest : Test
splitTest =
    suite "splitTest"

    [ test "'Abc-efgh' should give split Abc-/*" <|
      assertEqual
      "Abc-"
      (split "Abc-efgh" |> half1)

    , test "'Ab-defg' should give split Ab-/*" <|
      assertEqual
      "Ab-"
      (split "Ab-defg" |> half1)

    , test "'Ab-defgh' should give split Ab-/*" <|
      assertEqual
      "Ab-"
      (split "Ab-defgh" |> half1)

    , test "'A-cdefgh' should give split A-/*" <|
      assertEqual
      "A-"
      (split "A-cdefgh" |> half1)

    , test "'Abc efgh' should give split Abc/*" <|
      assertEqual
      "Abc"
      (split "Abc efgh" |> half1)

    , test "'Abc(tab)efgh' should give split Abc/*" <|
      assertEqual
      "Abc"
      (split "Abc\tefgh" |> half1)

    , test "'Ab(tab)(tab)efgh' should give split Ab/*" <|
      assertEqual
      "Ab"
      (split "Ab\t\tefgh" |> half1)

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

    , test "'Cat (newline) five' should give Cat/whitespace/five" <|
      assertEqual
      [ Block "Cat", Whitespace "\n", Block "five" ]
      (toParts "Cat\nfive")

    , test "'Cat (return) five' should give Cat/whitespace/five" <|
      assertEqual
      [ Block "Cat", Whitespace "\r", Block "five" ]
      (toParts "Cat\rfive")

    , test "'Cat-five' should give Cat-/five" <|
      assertEqual
      [ Block "Cat-", Block "five" ]
      (toParts "Cat-five")

    , test "'Cat--five' should give Cat-/-/five" <|
      assertEqual
      [ Block "Cat-", Block "-", Block "five" ]
      (toParts "Cat--five")

    , test "'-five' should give -/five" <|
      assertEqual
      [ Block "-", Block "five" ]
      (toParts "-five")

    ]

