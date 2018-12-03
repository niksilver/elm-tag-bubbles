module UtilTest exposing (all)

import Util exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "MultiBubbles suite"
    [ takeFirstJustTest
    , pairWithTest
    ]


takeFirstJustTest : Test
takeFirstJustTest =
  describe "takeFirstJustTest"

  [ test "On an empty list should be nothing" <|
    \_ ->
      Expect.equal
        Nothing
        (takeFirstJust [])

  , test "On a list of Nothings should be nothing" <|
    \_ ->
      Expect.equal
        Nothing
        (takeFirstJust [Nothing, Nothing, Nothing])

  , test "With just one in the middle should be that one" <|
    \_ ->
      Expect.equal
        (Just "b")
        (takeFirstJust [Nothing, Just "b", Nothing, Nothing])

  , test "With one at start and end should be the first one" <|
    \_ ->
      Expect.equal
        (Just "a")
        (takeFirstJust [Just "a", Nothing, Nothing, Just "d"])

  ]


pairWithTest : Test
pairWithTest =
  describe "pairWithTest"

  [ test "b paired with a should be (a, b)" <|
    \_ ->
      Expect.equal
      ("a", "b")
      ("a" |> pairWith "b")

  , test "2 paired with 1 should be (1, 2)" <|
    \_ ->
      Expect.equal
      (1, 2)
      (1 |> pairWith 2)

  ]

