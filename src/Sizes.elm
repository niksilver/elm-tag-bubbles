module Sizes exposing (toDict, rescale, idDict, topN, filter)


import Constants exposing (Id, Tag, Tags)

import Dict exposing (Dict)
import List
import Maybe exposing (withDefault)


-- Count the number of times each bubble appears

toDict : List Tags -> Dict Id Int
toDict listTags =
  List.foldl includeTags Dict.empty listTags


-- Include all the tags in a dict of tag counts

includeTags : List Tag -> Dict Id Int -> Dict Id Int
includeTags tags dict =
  List.foldl includeTag dict tags


-- Incude a tag in a dict of tag counts

includeTag : Tag -> Dict Id Int -> Dict Id Int
includeTag tag dict =
  Dict.update tag.id inc dict


inc : Maybe Int -> Maybe Int
inc mCount =
  case mCount of
    Just count -> Just (count + 1)
    Nothing -> Just 1


-- Rescale the dict according to max and min values

rescale : Float -> Float -> Dict Id Int -> Dict Id Float
rescale minSize maxSize dict =
  let
      counts = Dict.values dict
      minCount = List.minimum counts |> withDefault 0 |> toFloat
      maxCount = List.maximum counts |> withDefault 0 |> toFloat
      scale = (maxSize - minSize) / (maxCount - minCount)
      trans id count =
        maxSize - (maxCount - toFloat count) * scale
  in
      Dict.map trans dict


-- Get a mapping from id to tag

idDict : List Tags -> Dict Id Tag
idDict listTags =
  List.concat listTags
  |> List.map (\tag -> (tag.id, tag))
  |> Dict.fromList


-- Pick out the top N most frequently used tags

topN : Int -> List Tags -> List Id
topN n listTags =
  toDict listTags
  |> Dict.toList
  |> List.sortBy (\idCount -> -1 * (Tuple.second idCount))
  |> List.take n
  |> List.map Tuple.first


-- Filter in only those tags with the given names

filter : List Tags -> List Id -> List Tags
filter listTags ids =
  listTags
  |> List.map (reduce ids)
  |> List.filter (not << List.isEmpty)


reduce : List Id -> List Tag -> List Tag
reduce ids tags =
  List.filter (\tag -> List.member tag.id ids) tags


