module Main where

import Test
import Text
import Graphics.Element exposing (..)
import ElmTest exposing (..)


main : Element
main = 
    elementRunner Test.suite3
