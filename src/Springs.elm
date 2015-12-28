module Springs (counter) where

import Constants exposing (Tag, Tags)
import PairCounter exposing (Counter, emptyCounter, allPairs, inc)

import List

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

