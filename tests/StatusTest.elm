module StatusTest (all) where

import Status exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "StatusTest"
    [ updateTest
    ]

updateTest : Test
updateTest =
    suite "updateTest"

    [ test "Updating the rollover should add just the rollover" <|
      assertEqual
      (Status (Just "over!") (Just "Initial"))
      (Status Nothing (Just "Initial") |> update (Rollover "over!"))

    , test "Cancelling the rollover should remove it" <|
      assertEqual
      (Status Nothing (Just "Initial"))
      (Status (Just "oy!") (Just "Initial") |> update NoRollover)

    , test "Updaing the main message should change just that" <|
      assertEqual
      (Status (Just "oy!") (Just "replaced"))
      (Status (Just "oy!") (Just "Initial") |> update (Main "replaced"))

    , test "Cancelling the main message should change just it" <|
      assertEqual
      (Status (Just "oy!") Nothing)
      (Status (Just "oy!") (Just "Initial") |> update NoMain)

    ]

