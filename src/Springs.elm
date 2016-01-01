module Springs
    ( counter, lengths
    , acceleration, accelDict
    ) where

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

{-| Calculate the acceleration for a bubble.
    Parameters are:
    strength of the springs;
    the springs `Dict`;
    a bubble that's pulling the bubble in question;
    the bubble in question.
    Return value the acceleration in the x and y directions.
-}

acceleration : Float -> Dict (Id,Id) Float -> Bubble.Model -> Bubble.Model ->
    (Float, Float)
acceleration strength springs bubble2 bubble1 =
    case Dict.get (bubble1.id, bubble2.id) springs of
        Nothing ->
            (0, 0)
        Just springLength ->
            let
                mass = bubble1.size ^ 2
                -- If the two bubbles are on the same spot
                -- pretend the smaller-named one is one pixel to the
                -- left and up
                bubble1x =
                    if (bubble1.x == bubble2.x
                        && bubble1.y == bubble2.y
                        && bubble1.id < bubble2.id) then
                        bubble1.x - 1
                    else 
                        bubble1.x
                bubble1y =
                    if (bubble1.x == bubble2.x
                        && bubble1.y == bubble2.y
                        && bubble1.id < bubble2.id) then
                        bubble1.y - 1
                    else 
                        bubble1.y
                bubble2x =
                    if (bubble1.x == bubble2.x
                        && bubble1.y == bubble2.y
                        && bubble2.id < bubble1.id) then
                        bubble2.x - 1
                    else 
                        bubble2.x
                bubble2y =
                    if (bubble1.x == bubble2.x
                        && bubble1.y == bubble2.y
                        && bubble2.id < bubble1.id) then
                        bubble2.y - 1
                    else 
                        bubble2.y

                bubbleXDistance = bubble1x - bubble2x
                bubbleYDistance = bubble1y - bubble2y
                bubbleDistance = sqrt (bubbleXDistance^2 + bubbleYDistance^2)

                springXLength = bubbleXDistance / bubbleDistance * springLength
                springXExtension = bubbleXDistance - springXLength
                accelX = -strength * springXExtension / mass

                springYLength = bubbleYDistance / bubbleDistance * springLength
                springYExtension = bubbleYDistance - springYLength
                accelY = -strength * springYExtension / mass
            in
                (accelX, accelY)

{-| Return a dictionary of (x,y) acceleration for each bubble, identified
    by its id. Parameters are:
    a list of bubbles;
    a function which takes a pulling bubble and a pulled bubble and returns
    the pulled bubble's consequential acceleration.
-}

accelDict : List Bubble.Model ->
    (Bubble.Model -> Bubble.Model -> (Float, Float)) ->
    (Dict Id (Float, Float))
accelDict bubbles accelFun =
    Dict.empty
        |> Dict.insert "b1" (3, 0)
        {-let
        
    allPairs bubbles
        |> buildAccelPairs' accelFun
        |> addAccelPairs'

buildAccelPairs' : (Bubble.Model -> Bubble.Model -> (Float, Float)) ->
    List (Bubble.Model, Bubble.Model) ->
    Dict (Bubble.Model, Bubble.Model) (Float, Float)
buildAccelPairs' accelFun pairs =
    let
        rev (a,b) = (b,a)
        pairsRev = List.map rev pairs
        pairsPlus = pairs ++ pairsRev
    in
        List.map (\b1 b2 -> 
        -}
