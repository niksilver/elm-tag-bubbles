module UtilTest exposing (all)

import Util exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "MultiBubbles suite"
    [ pairWithTest
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

