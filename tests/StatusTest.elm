module StatusTest exposing (all)

import Status exposing (..)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "StatusTest suite"
    [ updateTest
    ]

updateTest : Test
updateTest =
    describe "updateTest"

    [ test "Updating the rollover should add just the rollover" <|
      \_ ->
        Expect.equal
        (Status (Just "over!") (Just "Initial"))
        (Status Nothing (Just "Initial") |> update (Rollover "over!"))

    , test "Cancelling the rollover should remove it" <|
      \_ ->
        Expect.equal
        (Status Nothing (Just "Initial"))
        (Status (Just "oy!") (Just "Initial") |> update NoRollover)

    , test "Updaing the main message should change just that" <|
      \_ ->
        Expect.equal
        (Status (Just "oy!") (Just "replaced"))
        (Status (Just "oy!") (Just "Initial") |> update (Main "replaced"))

    , test "Cancelling the main message should change just it" <|
      \_ ->
        Expect.equal
        (Status (Just "oy!") Nothing)
        (Status (Just "oy!") (Just "Initial") |> update NoMain)

    ]

