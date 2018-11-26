module LabelTest exposing (all)

import Label exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "LabelTest suite"
    [ pixelsToCharsTest
    , splitTest1
    , splitTest2
    , splitTest3
    , splitTest4
    , splitTest5
    , bestLengthTest
    , splitAndScaleTest
    , toPercentTest
    ]

pixelsToCharsTest : Test
pixelsToCharsTest =
    describe "pixelsToCharsTest"

    [ test "32 pixels should be 4 chars from trying it out on the screen" <|
      \_ ->
        Expect.equal
        4
        (pixelsToChars 32)

    , test "80 pixels should be 10 chars, from trying it out on the screen" <|
      \_ ->
        Expect.equal
        10
        (pixelsToChars 80)

    ]


half1 : Split -> String
half1 split =
    case split of
        Halves (a, _) -> a
        Whole _ -> "Error - got a whole looking for half1"


half2 : Split -> String
half2 split =
    case split of
        Halves (_, b) -> b
        Whole _ -> "Error - got a whole looking for half2"


splitTest1 : Test
splitTest1 =
    describe "splitTest - left half of a split"

    [ test "'Abc-efgh' should give split Abc-/*" <|
      \_ ->
        Expect.equal
        "Abc-"
        (split "Abc-efgh" |> half1)

    , test "'Ab-defg' should give split Ab-/*" <|
      \_ ->
        Expect.equal
        "Ab-"
        (split "Ab-defg" |> half1)

    , test "'Ab-defgh' should give split Ab-/*" <|
      \_ ->
        Expect.equal
        "Ab-"
        (split "Ab-defgh" |> half1)

    , test "'A-cdefgh' should give split A-/*" <|
      \_ ->
        Expect.equal
        "A-"
        (split "A-cdefgh" |> half1)

    , test "'Abc efgh' should give split Abc/*" <|
      \_ ->
        Expect.equal
        "Abc"
        (split "Abc efgh" |> half1)

    , test "'Abc(tab)efgh' should give split Abc/*" <|
      \_ ->
        Expect.equal
        "Abc"
        (split "Abc\tefgh" |> half1)

    , test "'Ab(tab)(tab)efgh' should give split Ab/*" <|
      \_ ->
        Expect.equal
        "Ab"
        (split "Ab\t\tefgh" |> half1)

    ]

splitTest2 : Test
splitTest2 =
    describe "splitTest - right half of a split if we could split in the first half"

    [ test "'Abc-efgh' should give split */efgh" <|
      \_ ->
        Expect.equal
        "efgh"
        (split "Abc-efgh" |> half2)

    , test "'Ab-defgh' should give split */defgh" <|
      \_ ->
        Expect.equal
        "defgh"
        (split "Ab-defgh" |> half2)

    , test "'Ab defgh' should give split */defgh" <|
      \_ ->
        Expect.equal
        "defgh"
        (split "Ab defgh" |> half2)

    , test "'Ab(spc)(spc)(spc)fgh' should give split */fgh" <|
      \_ ->
        Expect.equal
        "fgh"
        (split "Ab   fgh" |> half2)

    ]

splitTest3 : Test
splitTest3 =
    describe "splitTest - Both halves of a split if we couldn't split in the first half"

    [ test "'Abcdefgh' should give split ''/Abcdefgh" <|
      \_ ->
        Expect.equal
        (Whole "Abcdefgh")
        (split "Abcdefgh")

    , test "'Abcd-fgh' should give split Abcd-/fgh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcd-", "fgh"))
        (split "Abcd-fgh")

    , test "'Abcde-gh' should give split Abcde-/gh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcde-", "gh"))
        (split "Abcde-gh")

    , test "'Abcd fgh' should give split Abcd/fgh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcd", "fgh"))
        (split "Abcd fgh")

    , test "'Abcde gh' should give split Abcde/gh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcde", "gh"))
        (split "Abcde gh")

    , test "'Abcd(tab)fgh' should give split Abcd/fgh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcd", "fgh"))
        (split "Abcd\tfgh")

    , test "'Abcde(tab)gh' should give split Abcde/gh" <|
      \_ ->
        Expect.equal
        (Halves ("Abcde", "gh"))
        (split "Abcde\tgh")

    ]


splitTest4 : Test
splitTest4 =
    describe "splitTest - First half can be split but it's better to split in the seond half"

    [ test "'ooo ooo oooo' should give split ooo ooo/oooo" <|
      \_ ->
        Expect.equal
        (Halves ("ooo ooo", "oooo"))
        (split "ooo ooo oooo")

    ]

splitTest5 : Test
splitTest5 =
    describe "splitTest4 - split includes dashes and spaces and other oddities"

    [ test "'A - efghij' should split as A -/efghij" <|
      \_ ->
        Expect.equal
        (Halves ("A -", "efghij"))
        (split "A - efghij")

    , test "'Abcdef - j' should split as Abcdef/- j" <|
      \_ ->
        Expect.equal
        (Halves ("Abcdef", "- j"))
        (split "Abcdef - j")

    , test "'----' (four dashes) should split as dashdash/dashdash" <|
      \_ ->
        Expect.equal
        (Halves ("--", "--"))
        (split "----")

    , test "Four spaces should split as whole ''" <|
      \_ ->
        Expect.equal
        (Whole "")
        (split "")

    ]

bestLengthTest : Test
bestLengthTest =
    describe "bestLengthTest"

    [ test "Long+short word label should have length of long word" <|
      \_ ->
        Expect.equal
        6
        (bestLength (Halves ("Houses", "pot")))

    , test "Short+long word label should have length of long word" <|
      \_ ->
        Expect.equal
        8
        (bestLength (Halves ("Small", "fraction")))

    , test "Single word label should have length of that word" <|
      \_ ->
        Expect.equal
        9
        (bestLength (Whole "Operation"))

    ]

splitAndScaleTest : Test
splitAndScaleTest =
    describe "splitAndScaleTest"

    [ test "Small phrase should stay whole and have no scaling" <|
      \_ ->
        Expect.equal
        (Whole "Oil", 1)
        (splitAndScale "Oil" 10)

    , test "Very long word should stay whole and scale" <|
      \_ ->
        Expect.equal
        (Whole "Business", (6/8))
        (splitAndScale "Business" 6)

    , test "Long but broken label should split and scale to long word" <|
      \_ ->
        Expect.equal
        (Halves ("Business", "money"), (4/8))
        (splitAndScale "Business money" 4)

    , test "Long two-word label should split but not scale if space is wide enough for larger half" <|
      \_ ->
        Expect.equal
        (Halves ("Business", "money"), 1.0)
        (splitAndScale "Business money" 10)

    ]


toPercentTest : Test
toPercentTest =
    describe "toPercentText"

    [ test "1.0 -> 100%" <|
      \_ ->
        Expect.equal
        "100%"
        (toPercent 1.0)

    , test "0.8 -> 80%" <|
      \_ ->
        Expect.equal
        "80%"
        (toPercent 0.8)

    ]
