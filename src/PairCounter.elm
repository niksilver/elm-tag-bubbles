module PairCounter where

import Maybe exposing (withDefault)
import Dict exposing (..)

-- Something that tracks counts of pairs of records which have a String id.

type alias Idable a = { a | id : String }

type Counter = Counter (Dict (String, String) Int)

emptyDict : Dict (String, String) Int
emptyDict = empty

emptyCounter : Counter
emptyCounter = Counter emptyDict

-- Return the count of the given pair

countOf : Idable a -> Idable a -> Counter -> Int
countOf x y (Counter dict) =
    dict |> get (x.id, y.id) |> withDefault 0

-- Increment a counter

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

