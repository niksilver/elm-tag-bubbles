module Main where

import Test
import Graphics.Element exposing (Element)
import ElmTest exposing (elementRunner)


main : Element
main = 
    elementRunner Test.suite3
