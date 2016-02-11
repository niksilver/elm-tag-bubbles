module StatusTest (all) where

import Status exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "StatusTest"
    [ messageTest
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
