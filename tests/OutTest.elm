module OutTest exposing (all)

import Out exposing (..)

import Help exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "OutTest suite"
    [ takeFirstActualMsgTest
    ]


takeFirstActualMsgTest : Test
takeFirstActualMsgTest =
  describe "takeFirstActualMsgTest"

  [ test "On an empty list should be nothing" <|
    \_ ->
      Expect.equal
        Out.None
        (takeFirstActualMsg [])

  , test "On a list of Out.Nones should be nothing" <|
    \_ ->
      Expect.equal
        Out.None
        (takeFirstActualMsg [Out.None, Out.None, Out.None])

  , test "With just one in the middle should be that one" <|
    \_ ->
      Expect.equal
        (Out.HelpMsg On)
        (takeFirstActualMsg [Out.None, Out.HelpMsg Help.On, Out.None, Out.None])

  , test "With one at start and end should be the first one" <|
    \_ ->
      Expect.equal
        (Out.HelpMsg Off)
        (takeFirstActualMsg [Out.HelpMsg Help.Off, Out.None, Out.None, Out.HelpMsg Help.On])

  ]


