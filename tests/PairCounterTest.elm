module PairCounterTest (all) where

import PairCounter exposing (..)

import ElmTest exposing (..)

u = { id = "u" }
v = { id = "v" }
w = { id = "w" }
x = { id = "x" }
y = { id = "y" }
z = { id = "z" }

all : Test
all =
    suite "PairCounterTest"
    [ counterTest
    , allPairsTest
    , setTest
    , maxCountTest
    , minCountTest
    , sizeTest
    , topNTest
    ]

counterTest : Test
counterTest =
    suite "Counter functions"

    [ test "Empty counter should give zero for anything"
      (assertEqual
        0
        (countOf x y emptyCounter))

    , test "Should be able to add to an empty counter and see what's added"
      (assertEqual
        1
        (emptyCounter |> inc x y |> countOf x y))

    , test
      "Should be able to add to an empty counter twice and see what's added"
      (assertEqual
        2
        (emptyCounter |> inc x y |> inc x y |> countOf x y))
 
    , test
      "Should be able to add to an empty counter and other pairs are still zero"
      (assertEqual
        0
        (emptyCounter |> inc x y |> countOf x z))
 
    , test
      "Incrementing x y should show up in the count of y x"
      (assertEqual
        1
        (emptyCounter |> inc x y |> countOf y x))
 
    , test
      "Incrementing x y should show up in the count of y x"
      (assertEqual
        1
        (emptyCounter |> inc x y |> countOf y x))

    ]

setTest : Test
setTest =
    suite "setTest"

    [ test "Setting 3 on an empty counter should yield 3" <|
      assertEqual
      3
      (emptyCounter |> set x y 3 |> countOf x y)

    , test "Setting 3 on an empty counter should yield 3 on the pair reversed" <|
      assertEqual
      3
      (emptyCounter |> set x y 3 |> countOf y x)

    ]

allPairsTest : Test
allPairsTest =
    suite "allPairs"

    [ test "All pairs in [] should be []"
      (assertEqual
        []
        (allPairs []))

    , test "All pairs in [4] should be []"
      (assertEqual
        []
        (allPairs [4]))

    , test "All pairs in [6,7] should be [(6,7)]"
      (assertEqual
        [(6,7)]
        (allPairs [6, 7]))

    , test "All pairs in [3,4,5] should be [(3,4), (3,5), (4,5)]"
      (assertEqual
        [(3,4), (3,5), (4,5)]
        (allPairs [3, 4, 5]))

    , test
      "All pairs in [3,4,5,6] should be [(3,4), (3,5), (3,6), (4,5), (4,6), (5,6)]"
      (assertEqual
        [(3,4), (3,5), (3,6), (4,5), (4,6), (5,6)]
        (allPairs [3, 4, 5, 6]))
    ]

maxCountTest : Test
maxCountTest =
    suite "maxCount"

    [ test "maxCount of emptyCounter should be zero" <|
        assertEqual
        0
        (maxCount emptyCounter)

    , test "maxCount of a once-incremented counter should be 1" <|
        assertEqual
        1
        (emptyCounter |> inc x y |> maxCount)

    , test "maxCount with a 1 and 2 should should be 2" <|
        assertEqual
        2
        (emptyCounter |> inc x z |> inc x y |> inc z x |> maxCount)
    ]

minCountTest : Test
minCountTest =
    suite "minCount"

    [ test "minCount of emptyCounter should be zero" <|
        assertEqual
        0
        (minCount emptyCounter)

    , test "minCount of a once-incremented counter should be 1" <|
        assertEqual
        1
        (emptyCounter |> inc x y |> minCount)

    , test "minCount with a 1 and 2 should should be 1" <|
        assertEqual
        1
        (emptyCounter |> inc x z |> inc x y |> inc z x |> minCount)

    , test "minCount with a 3 and 2 should should be 2" <|
        assertEqual
        2
        (emptyCounter
            |> inc x z |> inc x z |> inc x z
            |> inc x y |> inc x y
            |> minCount)

    ]

sizeTest : Test
sizeTest =
    suite "sizeTest"

    [ test "Size of an empty counter should be zero" <|
      assertEqual
      0
      (size emptyCounter)

    , test "Size of a counter with one pair should be 1" <|
      assertEqual
      1
      (emptyCounter |> inc x y |> size)

    , test "Size of a counter with two pairs should be 2" <|
      assertEqual
      2
      (emptyCounter |> inc x y |> inc y z |> size)

    ]

topNTest : Test
topNTest =
    suite "topNTest"

    [ test
      "Top 3 counter from a counter with 10, 9, 8, 7 as counts should have size 3" <|
      assertEqual
      3
      (emptyCounter
        |> set u v 10
        |> set v w 9
        |> set w x 8
        |> set x y 7
        |> topN 3
        |> size)

    , test
      "Top 2 counter from a counter with 10, 9, 8, 7 as counts should have size 2" <|
      assertEqual
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
      assertEqual
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
      assertEqual
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
      assertEqual
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
      assertEqual
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
      assertEqual
      10
      (emptyCounter
        |> set u v 7
        |> set v w 8
        |> set w x 9
        |> set x y 10
        |> topN 2
        |> countOf x y)

    ]

