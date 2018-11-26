module PairCounterTest exposing (all)


import PairCounter exposing (..)

import Dict

import Test exposing (..)
import Expect exposing (..)


u = { id = "u" }
v = { id = "v" }
w = { id = "w" }
x = { id = "x" }
y = { id = "y" }
z = { id = "z" }


all : Test
all =
    describe "PairCounterTest suite"
    [ incTest
    , allPairsTest
    , setTest
    , toDictTest
    , maxCountTest
    , minCountTest
    , sizeTest
    , missingPairsTest
    , includeMissingPairsTest
    , topNTest
    ]


incTest : Test
incTest =
    describe "incTest"

    [ test "Empty counter should give zero for anything" <|
      \_ ->
        (Expect.equal
          0
          (countOf x y emptyCounter))

    , test "Should be able to add to an empty counter and see what's added" <|
      \_ ->
        (Expect.equal
          1
          (emptyCounter |> inc x y |> countOf x y))

    , test "Should be able to add to an empty counter twice and see what's added" <|
      \_ ->
        (Expect.equal
          2
          (emptyCounter |> inc x y |> inc x y |> countOf x y))
   
    , test "Should be able to add to an empty counter and other pairs are still zero" <|
      \_ ->
        (Expect.equal
          0
          (emptyCounter |> inc x y |> countOf x z))

    ]

setTest : Test
setTest =
    describe "setTest"

    [ test "Setting 3 on an empty counter should yield 3" <|
      \_ ->
        Expect.equal
        3
        (emptyCounter |> set x y 3 |> countOf x y)

    , test "Setting 3 on an empty counter should yield 3 on the pair reversed" <|
      \_ ->
        Expect.equal
        3
        (emptyCounter |> set x y 3 |> countOf y x)

    ]

allPairsTest : Test
allPairsTest =
    describe "allPairs"

    [ test "All pairs in [] should be []" <|
      \_ ->
        (Expect.equal
          []
          (allPairs []))

    , test "All pairs in [4] should be []" <|
      \_ ->
        (Expect.equal
          []
          (allPairs [4]))

    , test "All pairs in [6,7] should be [(6,7)]" <|
      \_ ->
        (Expect.equal
          [(6,7)]
          (allPairs [6, 7]))

    , test "All pairs in [3,4,5] should be [(3,4), (3,5), (4,5)]" <|
      \_ ->
        (Expect.equal
          [(3,4), (3,5), (4,5)]
          (allPairs [3, 4, 5]))

    , test "All pairs in [3,4,5,6] should be [(3,4), (3,5), (3,6), (4,5), (4,6), (5,6)]" <|
      \_ ->
        (Expect.equal
          [(3,4), (3,5), (3,6), (4,5), (4,6), (5,6)]
          (allPairs [3, 4, 5, 6]))
      ]

maxCountTest : Test
maxCountTest =
    describe "maxCount"

    [ test "maxCount of emptyCounter should be zero" <|
      \_ ->
          Expect.equal
          0
          (maxCount emptyCounter)

    , test "maxCount of a once-incremented counter should be 1" <|
      \_ ->
        Expect.equal
        1
        (emptyCounter |> inc x y |> maxCount)

    , test "maxCount with a 1 and 2 should should be 2" <|
      \_ ->
        Expect.equal
        2
        (emptyCounter |> inc x z |> inc x y |> inc z x |> maxCount)
    ]

minCountTest : Test
minCountTest =
    describe "minCount"

    [ test "minCount of emptyCounter should be zero" <|
      \_ ->
        Expect.equal
        0
        (minCount emptyCounter)

    , test "minCount of a once-incremented counter should be 1" <|
      \_ ->
        Expect.equal
        1
        (emptyCounter |> inc x y |> minCount)

    , test "minCount with a 1 and 2 should should be 1" <|
      \_ ->
        Expect.equal
        1
        (emptyCounter |> inc x z |> inc x y |> inc z x |> minCount)

    , test "minCount with a 3 and 2 should should be 2" <|
      \_ ->
        Expect.equal
        2
        (emptyCounter
            |> inc x z |> inc x z |> inc x z
            |> inc x y |> inc x y
            |> minCount)

    ]

sizeTest : Test
sizeTest =
    describe "sizeTest"

    [ test "Size of an empty counter should be zero" <|
      \_ ->
      Expect.equal
      0
      (size emptyCounter)

    , test "Size of a counter with one pair should be 1" <|
      \_ ->
      Expect.equal
      1
      (emptyCounter |> inc x y |> size)

    , test "Size of a counter with two pairs should be 2" <|
      \_ ->
      Expect.equal
      2
      (emptyCounter |> inc x y |> inc y z |> size)

    ]

missingPairsTest : Test
missingPairsTest =
    describe "missingPairsTest"

    [ test "Missing pairs from empty counter should be empty list" <|
      \_ ->
        Expect.equal
        []
        (emptyCounter |> missingPairs)

    , test "Missing pairs from counter with one pair should be empty list" <|
      \_ ->
        Expect.equal
        []
        (emptyCounter |> inc x y |> missingPairs)

    , test "Missing pairs from ctr with 2/3 pairs should reveal other pair" <|
      \_ ->
        Expect.equal
        [(x.id, z.id)]
        (emptyCounter |> inc x y |> inc y z |> missingPairs)

    , test "Missing pairs from ctr with 3/6 pairs should reveal other 3 pairs" <|
      \_ ->
        -- We have       We're missing
        --    u -- v         u    v
        --    |                \/ |
        --    |                /\ |
        --    w -- x         w    x
        Expect.equal
        [(u.id, x.id), (v.id, w.id), (v.id, x.id)]
        (emptyCounter
          |> inc u v |> inc u w |> inc w x
          |> missingPairs)

    ]

includeMissingPairsTest : Test
includeMissingPairsTest =
    describe "includeMissingPairsTest"

    [ test "Including missing pairs in empty counter should be empty counter" <|
      \_ ->
        Expect.equal
        0
        (emptyCounter |> includeMissingPairs |> size)

    , test "Including missing pairs in 1-pair ctr should be 1-pair ctr" <|
      \_ ->
        Expect.equal
        1
        (emptyCounter |> inc x y |> includeMissingPairs |> size)

    , test "Including missing pairs in ctr with 2/3 pairs should have 3 pairs" <|
      \_ ->
        Expect.equal
        3
        (emptyCounter
          |> inc x y |> inc y z |> includeMissingPairs |> size)

    , test "Including missing pairs in ctr with 2/3 pairs should include missing pair, even if converted to Dict" <|
      \_ ->
        Expect.equal
        (Just 0)
        (emptyCounter
            |> inc x y |> inc y z |> includeMissingPairs
            |> toDict |> Dict.get (x.id, z.id))

    , test "Including missing pairs in ctr with 2/3 pairs should retain old pair" <|
      \_ ->
        Expect.equal
        1
        (emptyCounter
            |> inc x y |> inc y z |> includeMissingPairs |> countOf x y)

    ]

topNTest : Test
topNTest =
    describe "topNTest"

    [ test "Top 3 counter from a counter with 10, 9, 8, 7 as counts should have size 3" <|
      \_ ->
        Expect.equal
        3
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 3
          |> size)

    , test "Top 2 counter from a counter with 10, 9, 8, 7 as counts should have size 2" <|
      \_ ->
        Expect.equal
        2
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 2
          |> size)

    , test
      ("Top 2 counter from a counter with 10, 9, 8, 7 as counts should " ++
       "contain top pair") <|
      \_ ->
        Expect.equal
        10
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 2
          |> countOf u v)

    , test
      ("Top 2 counter from a counter with 10, 9, 8, 7 as counts should " ++
       "contain top pair reversed") <|
      \_ ->
        Expect.equal
        10
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 2
          |> countOf v u)

    , test
      ("Top 2 counter from a counter with 10, 9, 8, 7 as counts should " ++
       "contain second pair") <|
      \_ ->
        Expect.equal
        9
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 2
          |> countOf v w)

    , test
      ("Top 2 counter from a counter with 10, 9, 8, 7 as counts should " ++
       "contain second pair reversed") <|
      \_ ->
        Expect.equal
        9
        (emptyCounter
          |> set u v 10
          |> set v w 9
          |> set w x 8
          |> set x y 7
          |> topN 2
          |> countOf w v)

    , test
      ("Top 2 counter from a counter with 7, 8, 9, 10 as counts should " ++
       "contain top pair") <|
      \_ ->
        Expect.equal
        10
        (emptyCounter
          |> set u v 7
          |> set v w 8
          |> set w x 9
          |> set x y 10
          |> topN 2
          |> countOf x y)

    ]

toDictTest : Test
toDictTest =
    describe "toDict"

    [ test "toDict of empty counter should be empty dict" <|
      \_ ->
        Expect.equal
        0
        (emptyCounter |> toDict |> Dict.size)

    , test "toDict of counter with one pair should contain pair" <|
      \_ ->
        Expect.equal
        (Just 1)
        (emptyCounter
          |> inc x y
          |> toDict
          |> Dict.get (x.id, y.id))

    , test "toDict of counter with one pair should contain pair reversed" <|
      \_ ->
        Expect.equal
        (Just 1)
        (emptyCounter
          |> inc x y
          |> toDict
          |> Dict.get (y.id, x.id))

    , test "toDict of counter with one pair should not contain another pair" <|
      \_ ->
        Expect.equal
        Nothing
        (emptyCounter
          |> inc x y
          |> toDict
          |> Dict.get (z.id, y.id))

    ]



