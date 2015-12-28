module PairCounter
    ( Idable, Counter
    , emptyCounter, countOf, inc
    , allPairs
    , maxCount
    )
    where

import List exposing (append, reverse)
import Maybe exposing (withDefault)
import Dict exposing (Dict, empty, get, insert)

-- Something that tracks counts of pairs of records which have a String id.
-- The order of pairs doesn't matter, so setting or getting x,y is the
-- same as that operation on y,x.

type alias Idable a = { a | id : String }

type Counter = Counter (Dict (String, String) Int)

emptyDict' : Dict (String, String) Int
emptyDict' = empty

emptyCounter : Counter
emptyCounter = Counter emptyDict'

-- Return the count of the given pair

countOf : Idable a -> Idable a -> Counter -> Int
countOf x y (Counter dict) =
    dict |> get (x.id, y.id) |> withDefault 0

-- Increment a counter.
-- Incrementing x,y will also increment y,x

inc : Idable a -> Idable a -> Counter -> Counter
inc x y counter =
    counter
        |> inc' x y
        |> inc' y x

inc' : Idable a -> Idable a -> Counter -> Counter
inc' x y (Counter dict as counter) =
    dict
        |> insert (x.id, y.id) (countOf x y counter + 1)
        |> Counter

-- Get all pairs (ignoring symmetry) of elements of a list.
-- It comes out in a predictable order (the order the numbers appear in
-- the original list) but that's implemented only for the convenience
-- of testing.

allPairs: List a -> List (a, a)
allPairs list =
    allPairs' (reverse list) [] []

allPairs' : List a -> List a -> List (a, a) -> List (a, a)
allPairs' next done accum =
    case next of
        hd :: tl ->
            allPairs' tl (hd :: done) (append (distribute' hd done) accum)
        [] ->
            accum

-- Distribute a seed across others to create pairs.
-- E.g. distribute 7 [1,2,3] == [(7,1), (7,2), (7,3)]

distribute' : a -> List a -> List (a, a)
distribute' seed others =
    List.map (\other -> (seed, other)) others

{-| Get the greatest count of all the pairs.
-}

maxCount : Counter -> Int
maxCount (Counter dict) =
    Dict.values dict |> List.maximum |> withDefault 0

