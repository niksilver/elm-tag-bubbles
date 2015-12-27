module CountCollector where

import Dict exposing (..)

-- Something that tracks counts of pairs of records which have a String id.

type alias Idable a = { a | id : String }

type Collector = Collector (Dict (String, String) Int)

emptyDict : Dict (String, String) Int
emptyDict = empty

emptyCollector : Collector
emptyCollector = Collector emptyDict

-- Return the count of the given pair

countOf : Collector -> Idable a -> Idable a -> Int
countOf coll o1 o2 =
    0
