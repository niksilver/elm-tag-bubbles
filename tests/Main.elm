module Main where

import TagFetcherTest exposing (all)
import CountCollectorTest exposing (all)

import Graphics.Element exposing (Element)
import ElmTest exposing (elementRunner, suite)


main : Element
main = 
    elementRunner
        (suite "All tests"
            [ TagFetcherTest.all
            , CountCollectorTest.all
            ]
        )
