module SpringsTest exposing (all)

import Springs exposing (..)
import Constants exposing (Tag, Tags)
import PairCounter exposing (countOf, emptyCounter, set)
import Bubble

import Maybe exposing (Maybe(..))
import Dict exposing (empty, insert)

import Test exposing (..)
import Expect exposing (..)


expectEqualTo4DP : Float -> Float -> Expectation
expectEqualTo4DP =
  Expect.within (Relative 0.0001)


tag1rec = Tag "world/bali-nine" "Bali Nine"


tag2rec = Tag "australia-news/q-a" "Q&amp;A"


tag3rec = Tag "environment/cecil-the-lion" "Cecil the lion"


tag4rec = Tag "world/zimbabwe" "Zimbabwe"


tag5rec = Tag "lifeandstyle/shops-and-shopping" "Shops and shopping"


tag6rec = Tag "money/consumer-affairs" "Consumer affairs"


tagListA : List Tags
tagListA =
    [ [ tag1rec, tag2rec, tag3rec ]
    , [ tag1rec, tag2rec, tag4rec ]
    , [ tag5rec, tag6rec ]
    ]


all : Test
all =
    describe "SpringsTest suite"
    [ toCounterTest
    , toDictTest
    , toDictWithZerosTest
    , accelerationTest
    , accelDictTest
    ]


toCounterTest : Test
toCounterTest =
    describe "toCounter"

    [ test "Two tags together twice should have a count of 2" <|
      \_ ->
        Expect.equal
        2
        (toCounter tagListA |> countOf tag1rec tag2rec)

    , test "Tags together twice should have a count of 2 the other way round" <|
      \_ ->
        Expect.equal
        2
        (toCounter tagListA |> countOf tag2rec tag1rec)

    , test "Tags not together should have a count of 0" <|
      \_ ->
        Expect.equal
        0
        (toCounter tagListA |> countOf tag3rec tag4rec)

    , test "Tags together once should have a count of 1" <|
      \_ ->
        Expect.equal
        1
        (toCounter tagListA |> countOf tag5rec tag6rec)

    ]


toDictTest : Test
toDictTest =
    describe "toDictTest"

    [ test "Pair with highest count should have the shortest length" <|
      \_ ->
        Expect.equal
        (Just 33.3)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDict 33.3 99.9
          |> Dict.get (tag1rec.id, tag2rec.id))

    , test "Pair with highest count should have the shortest length when reveresed" <|
      \_ ->
        Expect.equal
        (Just 33.3)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDict 33.3 99.9
          |> Dict.get (tag2rec.id, tag1rec.id))

    , test "Pair with lowest count should have the longest length" <|
      \_ ->
        Expect.equal
        (Just 99.9)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDict 33.3 99.9
          |> Dict.get (tag1rec.id, tag3rec.id))

    , test "Pair with middling count should have proportional length" <|
      \_ ->
        Expect.equal
        (Just (99 - 1 / 6 * 66))
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 4
          |> set tag2rec tag3rec 5   -- This count is 1/6 of the count range
          |> toDict 33 99
          |> Dict.get (tag2rec.id, tag3rec.id))

    , test "Pairs should have average length if all counts are the same" <|
      \_ ->
        Expect.equal
        (Just ((99 + 33)/2))
        (emptyCounter
          |> set tag1rec tag2rec 7
          |> set tag1rec tag3rec 7
          |> set tag2rec tag3rec 7
          |> toDict 33 99
          |> Dict.get (tag2rec.id, tag3rec.id))

    ]


toDictWithZerosTest : Test
toDictWithZerosTest =
    describe "toDictWithZerosTest"

    [ test "Pair with highest count should have the shortest length" <|
      \_ ->
        Expect.equal
        (Just 33.3)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDictWithZeros 33.3 99.9
          |> Dict.get (tag1rec.id, tag2rec.id))

    , test "Pair with highest count should have the shortest length when reveresed" <|
      \_ ->
        Expect.equal
        (Just 33.3)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDictWithZeros 33.3 99.9
          |> Dict.get (tag2rec.id, tag1rec.id))

    , test "Pair which is missing should have the longest length" <|
      \_ ->
        Expect.equal
        (Just 99.9)
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 3
          |> toDictWithZeros 33.3 99.9
          |> Dict.get (tag2rec.id, tag3rec.id))

    , test "Pair with middling count should have proportional length" <|
      \_ ->
        Expect.equal
        (Just (99 - 4/10 * 66))
        (emptyCounter
          |> set tag1rec tag2rec 10
          |> set tag1rec tag3rec 4   -- This count is 4/10 of the count range...
          |> toDictWithZeros 33 99   -- ...because we're including zero
          |> Dict.get (tag1rec.id, tag3rec.id))

    ]


accelerationTest : Test
accelerationTest =
    describe "accelerationTest"
    -- acceleration = (stiffness x distance) / mass
    -- mass is size squared

    [ test "Acceleration of bubble1 x-axis (too far right, stretched spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 60.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 480 - 400
            bubbleDistance = sqrt (80^2 + 50^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            (stiffness * -springXExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.first)

    , test "Acceleration of bubble1 x-axis (too far left, compressed spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 480 - 400
            bubbleDistance = sqrt (80^2 + 50^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            (stiffness * -springXExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.first)

    , test "Acceleration of bubble2 x-axis (too far left, stretched spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 60.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 400 - 480
            bubbleDistance = sqrt (80^2 + 50^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            (stiffness * -springXExtension / (60 * 60))
            (acceleration stiffness springs bubble1 bubble2 |> Tuple.first)

    , test "Acceleration of bubble2 x-axis (too far right, compressed spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 400 - 480
            bubbleDistance = sqrt (80^2 + 50^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            -- bubbleXDistance = -80
            -- springXLength = -100
            -- springXExtension = 20
            (stiffness * -springXExtension / (60 * 60))
            (acceleration stiffness springs bubble1 bubble2 |> Tuple.first)

    , test "Acceleration of bubble1 x-axis when bubble2 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 479 - 480
            bubbleDistance = sqrt (1^2 + 1^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            (stiffness * -springXExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.first)

    , test "Acceleration of bubble1 y-axis when bubble2 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleYDistance = 349 - 350
            bubbleDistance = sqrt (1^2 + 1^2)
            springYLength = bubbleYDistance / bubbleDistance * springLength
            springYExtension = bubbleYDistance - springYLength
        in
            expectEqualTo4DP
            (stiffness * -springYExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.second)

    , test "Acceleration of bubble2 x-axis when bubble1 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleXDistance = 480 - 479
            bubbleDistance = sqrt (1^2 + 1^2)
            springXLength = bubbleXDistance / bubbleDistance * springLength
            springXExtension = bubbleXDistance - springXLength
        in
            expectEqualTo4DP
            (stiffness * -springXExtension / (60 * 60))
            (acceleration stiffness springs bubble1 bubble2 |> Tuple.first)

    , test "Acceleration of bubble2 y-axis when bubble1 is same coord should be as if bubble1 is 1 pixel to the left and 1 pixel up" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleYDistance = 350 - 349
            bubbleDistance = sqrt (1^2 + 1^2)
            springYLength = bubbleYDistance / bubbleDistance * springLength
            springYExtension = bubbleYDistance - springYLength
        in
            expectEqualTo4DP
            (stiffness * -springYExtension / (60 * 60))
            (acceleration stiffness springs bubble1 bubble2 |> Tuple.second)

    , test "Acceleration of bubble1 y-axis (too far up, stretched spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 60.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleYDistance = 350 - 400
            bubbleDistance = sqrt (80^2 + 50^2)
            springYLength = bubbleYDistance / bubbleDistance * springLength
            springYExtension = bubbleYDistance - springYLength
        in
            expectEqualTo4DP
            -- bubbleYDistance = -50
            -- springYLength = -30
            -- springYExtension = -20
            (stiffness * -springYExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.second)

    , test "Acceleration of bubble1 y-axis (too far down, compressed spring)" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , dx = 0.0, dy = 0.0
                , size = 60, label = "Bubble 2"
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs =
                empty
                    |> insert ("b1", "b2") springLength
                    |> insert ("b2", "b1") springLength
            bubbleYDistance = 350 - 400
            bubbleDistance = sqrt (80^2 + 50^2)
            springYLength = bubbleYDistance / bubbleDistance * springLength
            springYExtension = bubbleYDistance - springYLength
        in
            expectEqualTo4DP
            -- bubbleYDistance = -50
            -- springYLength = -63
            -- springYExtension = 13
            (stiffness * -springYExtension / (80 * 80))
            (acceleration stiffness springs bubble2 bubble1 |> Tuple.second)

    , test "Acceleration of bubbles not linked should give zero acceleration" <|
      \_ ->
        let
            bubble1 =
                { id = "b1"
                , x = 480, y = 350
                , dx = 0.0, dy = 0.0
                , size = 80, label = "Bubble 1"
                , animation = Bubble.noAnimation
                }
            bubble2 =
                { id = "b2"
                , x = 400, y = 400
                , size = 60, label = "Bubble 2"
                , dx = 0.0, dy = 0.0
                , animation = Bubble.noAnimation
                }
            stiffness = 20.0
            springLength = 120.0
            springs = empty
        in
            Expect.equal
            (0, 0)
            (acceleration stiffness springs bubble2 bubble1)

    ]


accelDictTest : Test
accelDictTest =
    describe "accelDictTest"

    [ test ("Bubble1 pulled down equally, but never across, should have " ++
        "acceleration (3, 0)") <|
      \_ ->
        let
            bubbleX =
                { id = "n/a", x = 0, y = 0, dx = 0, dy = 0
                , size = 80, label = "X"
                , animation = Bubble.noAnimation
                }
            bubble1 = { bubbleX | id = "b1", label = "B1" }
            bubble2 = { bubbleX | id = "b2", label = "B2" }
            bubble3 = { bubbleX | id = "b3", label = "B3" }
            bubble4 = { bubbleX | id = "b4", label = "B4" }
            bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
            accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
            accelFun bubA bubB =
                case (bubA.id, bubB.id) of
                    ("b1", "b1") -> (0, 0)
                    ("b2", "b1") -> (1, 0)
                    ("b3", "b1") -> (1, 0)
                    ("b4", "b1") -> (1, 0)
                    _ -> (99, 99)
        in
            Expect.equal
            (Just (3, 0))
            (accelDict bubbles accelFun |> Dict.get "b1")

    , test ("Bubble1 pulled down unequally, but never across, should have " ++
        "acceleration (6, 0)") <|
      \_ ->
        let
            bubbleX =
                { id = "n/a", x = 0, y = 0, dx = 0, dy = 0
                , size = 80, label = "X"
                , animation = Bubble.noAnimation
                }
            bubble1 = { bubbleX | id = "b1", label = "B1" }
            bubble2 = { bubbleX | id = "b2", label = "B2" }
            bubble3 = { bubbleX | id = "b3", label = "B3" }
            bubble4 = { bubbleX | id = "b4", label = "B4" }
            bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
            accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
            accelFun bubA bubB =
                case (bubA.id, bubB.id) of
                    ("b1", "b1") -> (0, 0)
                    ("b2", "b1") -> (1, 0)
                    ("b3", "b1") -> (2, 0)
                    ("b4", "b1") -> (3, 0)
                    _ -> (99, 99)
        in
            Expect.equal
            (Just (6, 0))
            (accelDict bubbles accelFun |> Dict.get "b1")

    , test "Bubble2 pulled in different directions" <|
      \_ ->
        let
            bubbleX =
                { id = "n/a", x = 0, y = 0, dx = 0, dy = 0
                , size = 80, label = "X"
                , animation = Bubble.noAnimation
                }
            bubble1 = { bubbleX | id = "b1", label = "B1" }
            bubble2 = { bubbleX | id = "b2", label = "B2" }
            bubble3 = { bubbleX | id = "b3", label = "B3" }
            bubble4 = { bubbleX | id = "b4", label = "B4" }
            bubbles = [ bubble1, bubble2, bubble3, bubble4 ]
            accelFun : Bubble.Model -> Bubble.Model -> (Float, Float)
            accelFun bubA bubB =
                case (bubA.id, bubB.id) of
                    ("b1", "b2") -> (-10, 5)
                    ("b2", "b2") -> (0, 0)
                    ("b3", "b2") -> (1, 30)
                    ("b4", "b2") -> (2, -2)
                    _ -> (99, 99)
        in
            Expect.equal
            (Just (-7, 33))
            (accelDict bubbles accelFun |> Dict.get "b2")

    ]


