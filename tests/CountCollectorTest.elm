module CountCollectorTest where

import CountCollector exposing (..)

import ElmTest exposing (..)

x = { id = "x" }
y = { id = "y" }

all : Test
all =
    suite "CountCollectorTest"

    [ test "Empty collector should give zero for anything"
      (assertEqual
        0
        (countOf emptyCollector x y))
    ]
