module Springs exposing
  ( toCounter, toDict, toDictWithZeros
  , acceleration, accelDict
  , drag, dampen
  )


import Constants exposing
  ( Tag, Tags, Id
  , airDragFactor
  , minimumVelocity
  )
import PairCounter exposing
  ( Counter
  , emptyCounter, allPairs, inc
  , includeMissingPairs
  )
import Bubble

import List
import Dict exposing (Dict)
import Maybe exposing (withDefault)


-- Generate a `Counter` from a list of list of tags.

toCounter : List Tags -> Counter
toCounter tagsList =
  List.foldl includePairs emptyCounter tagsList


-- Given some tags and a pair counter, take all the pairings of the
-- tags and add them into the counter.

includePairs : List Tag -> Counter -> Counter
includePairs tags counter =
  List.foldl (\pair -> inc (Tuple.first pair) (Tuple.second pair)) counter (allPairs tags)


-- From a counter, generate a `Dict` from each tag id pair to its
-- respective spring length. The pair with the highest tag count
-- has the shortest spring length; the pair with the lowest has the
-- longest spring length. Any id pair is represented both ways
-- round. Shortest and longest lengths are the first
-- two parameters.

toDict : Float -> Float -> Counter -> Dict (String, String) Float
toDict shortest longest counter =
  let
      min = PairCounter.minCount counter |> toFloat
      max = PairCounter.maxCount counter |> toFloat
      countRange = max - min
      lengthRange = longest - shortest
      conv thisCount =
        if (countRange == 0) then
          shortest + (lengthRange / 2)
        else
          shortest + ((max - thisCount) / countRange) * lengthRange
  in
      PairCounter.toDict counter
      |> Dict.map (\pair count -> conv (toFloat count))


-- Just like toDict, but include any pairs which aren't actually connected
-- (i.e. are missing from the counts of pairs).

toDictWithZeros : Float -> Float -> Counter -> Dict (String, String) Float
toDictWithZeros shortest longest counter =
  counter
  |> includeMissingPairs
  |> toDict shortest longest


-- Calculate the acceleration for a bubble.
-- Parameters are:
-- strength of the springs;
-- the springs `Dict`;
-- a bubble that's pulling the bubble in question;
-- the bubble in question.
-- Return value the acceleration in the x and y directions.

acceleration : Float -> Dict (Id,Id) Float -> Bubble.Model -> Bubble.Model -> (Float, Float)
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


signedSqrt : Float -> Float
signedSqrt a =
  if a < 0 then
    -1 * (sqrt -a)
  else
    sqrt a


-- Return a dictionary of (x,y) acceleration for each bubble, identified
-- by its id. Parameters are:
-- a list of bubbles;
-- a function which takes a pulling bubble and a pulled bubble and returns
-- the pulled bubble's consequential acceleration.

accelDict : List Bubble.Model ->
  (Bubble.Model -> Bubble.Model -> (Float, Float)) ->
  (Dict Id (Float, Float))
accelDict bubbles accelFun =
  let
      allAccels : Bubble.Model -> List (Float, Float)
      allAccels bub =
        List.map (\otherBub -> accelFun otherBub bub) bubbles
      sumAccels : List (Float, Float) -> (Float, Float)
      sumAccels accels =
        List.foldl
          (\acc1 acc2 -> (Tuple.first acc1 + Tuple.first acc2, Tuple.second acc1 + Tuple.second acc2))
          (0, 0)
          accels
      accel : Bubble.Model -> (Id, (Float, Float))
      accel bub =
        (bub.id, allAccels bub |> sumAccels)
  in
      List.map accel bubbles
      |> Dict.fromList


-- Calculate the x and y drag given an initial dx and dy velocity.

drag : Float -> Float -> (Float, Float)
drag dx dy =
  let
      v = sqrt (dx^2 + dy^2)
      drag_ = -airDragFactor * v * v
      dragX = if v == 0 then 0 else drag_ * dx / v
      dragY = if v == 0 then 0 else drag_ * dy / v
      dragX_ = if abs dragX > abs dx then -dx else dragX
      dragY_ = if abs dragY > abs dy then -dy else dragY
  in
      (dragX_, dragY_)


-- Given a dx and dy velocity return the same values, or zero if the
-- overall velocity is too low.

dampen : Float -> Float -> (Float, Float)
dampen dx dy =
  let
      v = sqrt (dx^2 + dy^2)
      dx_ = if v < minimumVelocity then 0 else dx
      dy_ = if v < minimumVelocity then 0 else dy
  in
      (dx_, dy_)

