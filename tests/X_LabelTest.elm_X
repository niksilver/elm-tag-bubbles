module LabelTest (all) where

import Label exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "LabelTest"
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
    suite "pixelsToCharsTest"

    [ test "32 pixels should be 4 chars from trying it out on the screen" <|
      assertEqual
      4
      (pixelsToChars 32)

    , test "80 pixels should be 10 chars, from trying it out on the screen" <|
      assertEqual
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
    suite "splitTest - left half of a split"

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

splitTest2 : Test
splitTest2 =
    suite "splitTest - right half of a split if we could split in the first half"

    [ test "'Abc-efgh' should give split */efgh" <|
      assertEqual
      "efgh"
      (split "Abc-efgh" |> half2)

    , test "'Ab-defgh' should give split */defgh" <|
      assertEqual
      "defgh"
      (split "Ab-defgh" |> half2)

    , test "'Ab defgh' should give split */defgh" <|
      assertEqual
      "defgh"
      (split "Ab defgh" |> half2)

    , test "'Ab(spc)(spc)(spc)fgh' should give split */fgh" <|
      assertEqual
      "fgh"
      (split "Ab   fgh" |> half2)

    ]

splitTest3 : Test
splitTest3 =
    suite "splitTest - Both halves of a split if we couldn't split in the first half"

    [ test "'Abcdefgh' should give split ''/Abcdefgh" <|
      assertEqual
      (Whole "Abcdefgh")
      (split "Abcdefgh")

    , test "'Abcd-fgh' should give split Abcd-/fgh" <|
      assertEqual
      (Halves ("Abcd-", "fgh"))
      (split "Abcd-fgh")

    , test "'Abcde-gh' should give split Abcde-/gh" <|
      assertEqual
      (Halves ("Abcde-", "gh"))
      (split "Abcde-gh")

    , test "'Abcd fgh' should give split Abcd/fgh" <|
      assertEqual
      (Halves ("Abcd", "fgh"))
      (split "Abcd fgh")

    , test "'Abcde gh' should give split Abcde/gh" <|
      assertEqual
      (Halves ("Abcde", "gh"))
      (split "Abcde gh")

    , test "'Abcd(tab)fgh' should give split Abcd/fgh" <|
      assertEqual
      (Halves ("Abcd", "fgh"))
      (split "Abcd\tfgh")

    , test "'Abcde(tab)gh' should give split Abcde/gh" <|
      assertEqual
      (Halves ("Abcde", "gh"))
      (split "Abcde\tgh")

    ]

splitTest4 : Test
splitTest4 =
    suite "splitTest - First half can be split but it's better to split in the seond half"

    [ test "'ooo ooo oooo' should give split ooo ooo/oooo" <|
      assertEqual
      (Halves ("ooo ooo", "oooo"))
      (split "ooo ooo oooo")

    ]

splitTest5 : Test
splitTest5 =
    suite "splitTest4 - split includes dashes and spaces and other oddities"

    [ test "'A - efghij' should split as A -/efghij" <|
      assertEqual
      (Halves ("A -", "efghij"))
      (split "A - efghij")

    , test "'Abcdef - j' should split as Abcdef/- j" <|
      assertEqual
      (Halves ("Abcdef", "- j"))
      (split "Abcdef - j")

    , test "'----' (four dashes) should split as dashdash/dashdash" <|
      assertEqual
      (Halves ("--", "--"))
      (split "----")

    , test "Four spaces should split as whole ''" <|
      assertEqual
      (Whole "")
      (split "")

    ]

bestLengthTest : Test
bestLengthTest =
    suite "bestLengthTest"

    [ test "Long+short word label should have length of long word" <|
      assertEqual
      6
      (bestLength (Halves ("Houses", "pot")))

    , test "Short+long word label should have length of long word" <|
      assertEqual
      8
      (bestLength (Halves ("Small", "fraction")))

    , test "Single word label should have length of that word" <|
      assertEqual
      9
      (bestLength (Whole "Operation"))

    ]

splitAndScaleTest : Test
splitAndScaleTest =
    suite "splitAndScaleTest"

    [ test "Small phrase should stay whole and have no scaling" <|
      assertEqual
      (Whole "Oil", 1)
      (splitAndScale "Oil" 10)

    , test "Very long word should stay whole and scale" <|
      assertEqual
      (Whole "Business", (6/8))
      (splitAndScale "Business" 6)

    , test "Long but broken label should split and scale to long word" <|
      assertEqual
      (Halves ("Business", "money"), (4/8))
      (splitAndScale "Business money" 4)

    , test "Long two-word label should split but not scale if space is wide enough for larger half" <|
      assertEqual
      (Halves ("Business", "money"), 1.0)
      (splitAndScale "Business money" 10)

    ]


toPercentTest : Test
toPercentTest =
    suite "toPercentText"

    [ test "1.0 -> 100%" <|
      assertEqual
      "100%"
      (toPercent 1.0)

    , test "0.8 -> 80%" <|
      assertEqual
      "80%"
      (toPercent 0.8)

    ]
