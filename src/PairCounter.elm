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

countOf : Counter -> Idable a -> Idable a -> Int
countOf (Counter dict) o1 o2 =
    case (get (o1.id, o2.id) dict) of
        Just v -> v
        Nothing -> 0

-- Increment a counter

inc : Counter -> Idable a -> Idable a -> Counter
inc (Counter dict) x y =
    Counter (insert (x.id, y.id) 1 dict)
