module MultiBubbles exposing
    ( Msg (..)
    , Model
    , Diff, tagDiff
    , make
    , initialArrangement, arrangeCentre
    , Bounds, bounds, recentre, forNewDimensions
    , update, view
    )

-- Multiple bubbles

import Constants exposing (Id, Tag)
-- import Context exposing (Context, forwardTo)
import Bubble
import Springs exposing (drag, dampen)

import List exposing (reverse, length, indexedMap, map, append, filter, member)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Svg exposing (Svg)
import Time exposing (Posix)


type alias Model = List Bubble.Model


type Msg
    = Tick Posix
    | Direct Id Bubble.Message
    | AdjustVelocities (Dict Id (Float, Float))


-- The difference between tags already in the world and tags fetched
-- via the API.

type alias Diff a = { old : List a, new : List a, both : List a }


-- The bounds of some bubbles

type alias Bounds = { top : Float, right : Float, bottom : Float, left : Float }


-- Find the difference between tags represented in the world
-- and tags fetched by the API

tagDiff : List a -> List a -> Diff a
tagDiff current latest =
    let
        old = filter (\e -> not(member e latest)) current
        new = filter (\e -> not(member e current)) latest
        both = filter (\e -> member e current) latest
    in
        Diff old new both


-- Make some new bubbles using a dictionary of tags and a dictionary of sizes

make : List Tag -> Dict Id Float -> Model
make tags sizeDict =
    let
        size tag = Dict.get tag.id sizeDict |> withDefault 10.0
        mkBubble tag = Bubble.make tag (size tag)
    in
        List.map mkBubble tags


-- Create an initial arrangement for a number of new bubbles.
-- Old bubbles will fade out; new bubbles will fade in; remaining bubbles remain

initialArrangement : Float -> Float -> Model -> Model -> Model
initialArrangement centreX centreY oldModel newModel =
    arrangeCentre centreX centreY newModel
        |> replace oldModel
        |> reorder


arrangeCentre : Float -> Float -> Model -> Model
arrangeCentre centreX centreY model =
    let
        count = length model
        turn = 2 * pi / (toFloat count)
        rePos : Int -> Bubble.Model -> Bubble.Model
        rePos idx bubble =
            { bubble
            | x = centreX + 40 * (cos (turn * (toFloat idx)))
            , y = centreY + 40 * (sin (turn * (toFloat idx)))
            }
                |> Bubble.setToFadeIn
        indexedBubs = indexedMap Tuple.pair model
    in
        map (\ib -> rePos (Tuple.first ib) (Tuple.second ib)) indexedBubs


-- Find the bounds of the model

bounds : Model -> Bounds
bounds model =
    case model of
        [] ->
            Bounds 0 0 0 0
        hd :: tl ->
            boundsOne hd |> addBounds tl


addBounds : Model -> Bounds -> Bounds
addBounds model bds =
    case model of
        [] -> bds
        hd :: tl -> 
            addOne bds hd |> addBounds tl


addOne : Bounds -> Bubble.Model -> Bounds
addOne bds bub =
    let
        b2 = boundsOne bub
    in
        Bounds
        (min bds.top b2.top)
        (max bds.right b2.right)
        (max bds.bottom b2.bottom)
        (min bds.left b2.left)


boundsOne : Bubble.Model -> Bounds
boundsOne b =
    Bounds (b.y - b.size) (b.x + b.size) (b.y + b.size) (b.x - b.size)


-- Recentre the bubbles in a frame of the given dimensions

recentre : Model -> (Int, Int) -> Model
recentre model (w, h) =
    let
        -- window centre x, window centre y
        wcx = toFloat w / 2
        wcy = toFloat h / 2
        -- Bubble bounds centre x and y
        bds = bounds model
        bcx = (bds.left + bds.right) / 2
        bcy = (bds.top + bds.bottom) / 2
        -- Offset correction needed x and y
        offx = wcx - bcx
        offy = wcy - bcy
        -- Move a bubble
        move bubble =
            { bubble | x = bubble.x + offx, y = bubble.y + offy }
    in
        List.map move model


-- Shift the bubbles to accommodate new world dimensions

forNewDimensions : (Int, Int) -> (Int, Int) -> Model -> Model
forNewDimensions (oldX, oldY) (newX, newY) model =
    let
        dX = (toFloat newX - toFloat oldX) / 2
        dY = (toFloat newY - toFloat oldY) / 2
        move bubble =
            { bubble | x = bubble.x + dX, y = bubble.y + dY }
    in
        List.map move model


-- Replace an old model with a new model.

replace : Model -> Model -> Model
replace oldModel newModel =
    let
        diff = tagDiff (map .id oldModel) (map .id newModel)
    in
        oldModel
            |> fadeIn newModel diff.new
            |> fadeOut diff.old
            |> resize newModel diff.both


fadeIn : Model -> List Id -> Model -> Model
fadeIn newModel newIds oldModel =
    filter (\bubble -> member bubble.id newIds) newModel
        |> append oldModel


fadeOut : List Id -> Model -> Model
fadeOut oldIds oldModel =
    let
        selectedFade bubble =
            if (member bubble.id oldIds) then
                Bubble.setToFadeOut bubble
            else
                bubble
    in
        map selectedFade oldModel


resize : Model -> List Id -> Model -> Model
resize newModel ids model =
    let
        newBubbleSize id =
            find (\b -> b.id == id) newModel
                |> Maybe.map .size
                |> withDefault 10.0
        selectedResize bubble =
            if (member bubble.id ids) then
                Bubble.setToResize (newBubbleSize bubble.id) bubble
            else
                bubble
    in
        map selectedResize model


-- Find the first element in a list that passes a given test

find : (a -> Bool) -> List a -> Maybe a
find test list =
    List.filter test list |> List.head


-- Reorder the bubbles with the largest first

reorder : Model -> Model
reorder model =
    model
        |> List.sortBy (\bubble -> -1 * (Bubble.targetSize bubble))


-- Update the model

update : Msg -> Model -> Model
update msg model =
    case msg of
        Direct id bubAct -> updateOne id bubAct model
        Tick time -> updateAll model time
        AdjustVelocities accels -> updateVelocities accels model


updateOne : Id -> Bubble.Message -> Model -> Model
updateOne id bubMsg model =
    let
        selectiveUpdate bubModel =
            if id == bubModel.id then
                (Bubble.update bubMsg bubModel)
            else
                bubModel
    in
        map selectiveUpdate model


-- Update all the bubbles with the Tick action

updateAll : Model -> Posix -> Model
updateAll model time =
    map (Bubble.update (Bubble.Animate time)) model
        |> removeFadedOut
        |> cancelFinishedResizes


removeFadedOut : Model -> Model
removeFadedOut model =
    filter (Bubble.isFadedOut >> not) model


cancelFinishedResizes : Model -> Model
cancelFinishedResizes model =
    map Bubble.cancelFinishedResize model


-- Update the velocity of all the bubbles
-- according to a dictionary of id to acceleration

updateVelocities : Dict Id (Float, Float) -> Model -> Model
updateVelocities accels model =
    map (updateVelocity accels) model


updateVelocity : Dict Id (Float, Float) -> Bubble.Model -> Bubble.Model
updateVelocity accels bubble =
    let
        accelXY = Dict.get bubble.id accels |> withDefault (0, 0)
        accelX = Tuple.first accelXY
        accelY = Tuple.second accelXY
        dragXY = drag bubble.dx bubble.dy
        dragX = Tuple.first dragXY
        dragY = Tuple.second dragXY
        dx = bubble.dx + accelX + dragX
        dy = bubble.dy + accelY + dragY
        dampenedDxDy = dampen dx dy
    in
        { bubble | dx = Tuple.first dampenedDxDy, dy = Tuple.second dampenedDxDy }


-- A view of a Bubble, using an address at this level of the architecture

-- fwdingView : Context Action -> Bubble.Model -> Svg
-- fwdingView context bubble =
    -- Bubble.view (forwardTo context (Direct bubble.id)) bubble

view : Model -> List (Svg Msg)
view model =
  let
      mapper bubble =
        Bubble.view bubble
        |> Svg.map (Direct bubble.id)
  in
    map mapper model


