module Main where

import Test
import TagFetcherTest
import Graphics.Element exposing (Element)
import ElmTest exposing (elementRunner)


main : Element
main = 
    elementRunner TagFetcherTest.all
