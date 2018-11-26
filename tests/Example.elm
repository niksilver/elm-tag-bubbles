module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Some example tests"
    [ test "We know what a palindrom is" <|
      \_ ->
        let
            str = "hannah"
        in
            Expect.equal str (String.reverse str)
    ]



