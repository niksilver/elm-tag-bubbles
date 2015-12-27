module PairCounter where

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
countOf o1 o2 (Counter dict) =
    case (get (o1.id, o2.id) dict) of
        Just v -> v
        Nothing -> 0

-- Increment a counter

inc : Idable a -> Idable a -> Counter -> Counter
inc x y (Counter dict as counter) =
    Counter (insert (x.id, y.id) ((countOf x y counter)+1) dict)
