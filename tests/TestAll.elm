module TestAll where

import TagFetcherTest
import PairCounterTest
import SpringsTest
import SizesTest
import MultiBubblesTest
import WorldTest
import LabelTest
import StatusTest

import Graphics.Element exposing (Element)
import ElmTest exposing (elementRunner, suite)


main : Element
main = 
    elementRunner
        (suite "All tests"
            [ TagFetcherTest.all
            , PairCounterTest.all
            , SpringsTest.all
            , SizesTest.all
            , MultiBubblesTest.all
            , WorldTest.all
            , LabelTest.all
            , StatusTest.all
            ]
        )
