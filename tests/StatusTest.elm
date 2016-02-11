module StatusTest (all) where

import Status exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "StatusTest"
    [ messageTest
    , updateTest
    ]

messageTest : Test
messageTest =
    suite "messageTest"

    [ test "Status with no overlay message should show core message" <|
      assertEqual
      "Ready"
      (Status Nothing "Ready" |> message)

    , test "Status with an overlay message should show the overlay" <|
      assertEqual
      "Hold on"
      (Status (Just "Hold on") "Ready" |> message)

    ]

updateTest : Test
updateTest =
    suite "updateTest"

    [ test "Updating with an overlay should add just the overlay" <|
      assertEqual
      (Status (Just "over!") "Initial")
      (Status Nothing "Initial" |> update (Overlay "over!"))

    , test "Cancelling the overlay should remove it" <|
      assertEqual
      (Status Nothing "Initial")
      (Status (Just "oy!") "Initial" |> update NoOverlay)

    , test "Updaing the main message should change just that" <|
      assertEqual
      (Status (Just "oy!") "replaced")
      (Status (Just "oy!") "Initial" |> update (Main "replaced"))

    , test "Resetting the main message should change it & remove the overlay" <|
      assertEqual
      (Status Nothing "replaced")
      (Status (Just "oy!") "Initial" |> update (Reset "replaced"))

    ]

