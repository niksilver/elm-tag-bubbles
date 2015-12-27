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
countOf coll o1 o2 =
    0
