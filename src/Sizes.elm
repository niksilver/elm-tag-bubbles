module Sizes (toDict) where

import Constants exposing (Tag, Tags)

import Dict exposing (Dict)
import List
import Maybe exposing (withDefault)

-- Work out bubble sizes

toDict : Float -> Float -> List Tags -> Dict String Float
toDict min max listTags =
    List.foldl includeTags Dict.empty listTags
        |> rescale min max

-- Include all the tags in a dict of tag counts

includeTags : List Tag -> Dict String Int -> Dict String Int
includeTags tags dict =
    List.foldl includeTag dict tags

-- Incude a tag in a dict of tag counts

includeTag : Tag -> Dict String Int -> Dict String Int
includeTag tag dict =
    Dict.update tag.id inc dict

inc : Maybe Int -> Maybe Int
inc mCount =
    case mCount of
        Just count -> Just (count + 1)
        Nothing -> Just 1

-- Rescale the dict according to max and min values

rescale : Float -> Float -> Dict String Int -> Dict String Float
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

