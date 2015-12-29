module Springs (counter, lengths) where

import Constants exposing (Tag, Tags)
import PairCounter exposing (Counter, emptyCounter, allPairs, inc)

import List
import Dict exposing (Dict)

{-| Generate a `Counter` from a list of list of tags.
-}

counter : List Tags -> Counter
counter tagsList =
    List.foldl includePairs' emptyCounter tagsList

{-| Given some tags and a pair counter, take all the pairings of the
    tags and add them into the counter.
-}

includePairs' : List Tag -> Counter -> Counter
includePairs' tags counter =
    List.foldl (\pair -> inc (fst pair) (snd pair)) counter (allPairs tags)

{-| From a counter, generate a `Dict` from each tag id pair to its
    respective spring length. The pair with the highest tag count
    has the shortest spring length; the pair with the lowest has the
    longest spring length. Shortest and longest lengths are the first
    two parameters.
-}

lengths : Float -> Float -> Counter -> Dict (String, String) Float
lengths shortest longest counter =
    let
        min = PairCounter.minCount counter |> toFloat
        max = PairCounter.maxCount counter |> toFloat
        countRange = max - min
        lengthRange = longest - shortest
        conv thisCount =
            shortest + ((max - thisCount) / countRange) * lengthRange
    in
        PairCounter.toDict counter
        |> Dict.map (\pair count -> conv (toFloat count))

