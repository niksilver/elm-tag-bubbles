module ElmTestExtras (assertEqualTo4DP) where

import ElmTest exposing (Assertion, assertEqual)

assertEqualTo4DP : Float -> Float -> ElmTest.Assertion
assertEqualTo4DP expected actual =
    assertEqual
    ((expected * 10000 |> round |> toFloat ) / 10000)
    ((actual * 10000 |> round |> toFloat ) / 10000)

