module Springs (counter, lengths, acceleration) where

import Constants exposing (Tag, Tags, Id)
import PairCounter exposing (Counter, emptyCounter, allPairs, inc)
import Bubble

import List
import Dict exposing (Dict)
import Maybe exposing (withDefault)

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

{-| Calculate the acceleration along a given access for a bubble.
    Parameters are:
    stiffness of the springs;
    the springs `Dict`;
    the bubble that's pulling the bubble in question;
    the bubble in question.
-}

acceleration : Float -> Dict (Id,Id) Float -> Bubble.Model -> Bubble.Model -> Float
acceleration stiffness springs bubble2 bubble1 =
    let
        id1 = bubble1.id
        id2 = bubble2.id
        springLength = Dict.get (id1, id2) springs |> withDefault 0
        bubbleXDistance = bubble1.x - bubble2.x
        bubbleYDistance = bubble1.y - bubble2.y
        bubbleDistance = sqrt (bubbleXDistance^2 + bubbleYDistance^2)
        springXLength = bubbleXDistance / bubbleDistance * springLength
        springXExtension = bubbleXDistance - springXLength
        mass = bubble1.size ^ 2
    in
        -stiffness * springXExtension / mass

