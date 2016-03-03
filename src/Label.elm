module Label
    ( pixelsToChars
    , Split(Halves, Whole), split
    , leastLengthStr, splitAndScale
    , toPercent
    , view
    ) where

import Colours exposing (pickTextColour)

import String exposing
    ( cons, uncons, fromChar, left, length, right
    , dropRight, trimRight, dropLeft, trimLeft
    )
import Svg exposing (Svg)
import Svg.Attributes

-- Splitting a string into halves (if possible)

type Split = Halves (String, String) | Whole String

-- Breaking text into parts suitable for line breaks

type Part = Block String | Whitespace String

type PartChar = BlockChar Char | WhitespaceChar Char

-- How many characters can we fit into a pixel width?

pixelsToChars : Float -> Int
pixelsToChars px =
    px / 16 * 2  -- 1em = 16px, and the average character is narrower then 1em
        |> floor

-- Split a string (if possible)

split : String -> Split
split text =
    let
        diff (a, b) = (length a) - (length b) |> abs
        backSplit = splitBackward text
        foreSplit = splitForward text
    in
        (if (diff backSplit < diff foreSplit) then backSplit else foreSplit)
        |> toType

-- Split the text working backwards from the middle

splitBackward : String -> (String, String)
splitBackward text =
    let
        halfIdx = length text // 2
        front = left halfIdx text
        half1 = backSplitFrom front
        remainder = dropLeft (length half1) text
        half2 = trimLeft remainder
    in
        (half1, half2)

-- Work backwards from the end of a string to find where to split it

backSplitFrom : String -> String
backSplitFrom text =
    let
        trimmedRight = trimRight text
        isTrimmedRight = (length trimmedRight < length text)
    in
        if (isTrimmedRight) then
            trimmedRight
        else if (text == "") then
            ""
        else if (right 1 text == "-") then
            text
        else
            dropRight 1 text |> backSplitFrom

-- Split the text working forward from the middle

splitForward : String -> (String, String)
splitForward text =
    let
        halfIdx = length text // 2
        back = dropLeft halfIdx text
        half2 = frontSplitFrom back
        half1 = dropRight (length half2) text |> trimRight
    in
        (half1, half2)

-- Cut off the front of a string, working forward from the start

frontSplitFrom : String -> String
frontSplitFrom text =
    let
        trimmedLeft = trimLeft text
        isTrimmedLeft = (length trimmedLeft < length text)
    in
        if (text == "") then
            ""
        else if (left 1 text == "-") then
            dropLeft 1 text
        else if (isTrimmedLeft) then
            trimmedLeft
        else
            dropLeft 1 text |> frontSplitFrom

-- Turn a pair representing a split into a type

toType : (String, String) -> Split
toType (half1, half2) =
    if (half2 == "") then
        Whole half1
    else
        Halves (half1, half2)

-- Least length a split label can be (in characters) given it may be split

leastLength : Split -> Int
leastLength splt =
    case splt of
        Halves (a, b) -> max (length a) (length b)
        Whole a -> length a

-- Least length a string label can be (in characters) given it may be split

leastLengthStr : String -> Int
leastLengthStr text =
    split text
        |> leastLength

-- Given a label and a character width, decide if it should split and how
-- it should scale

splitAndScale : String -> Int -> (Split, Float)
splitAndScale text width =
    let
        spltLen = if (length text <= width) then
                (Whole text, length text)
            else
                let
                    splt = split text
                in
                    (splt, leastLength splt)
        splt = fst spltLen
        len = snd spltLen
    in
        if (len <= width) then
            (splt, 1.0)
        else
            (splt, (toFloat width / (len |> toFloat)))

-- Convert a float to a percentage string

toPercent : Float -> String
toPercent k =
    (k * 100 |> toString) ++ "%"

-- Convenience class for the Svg style attribute

styles : List (String, String) -> Svg.Attribute
styles pairs =
    pairs
        |> List.map (\p -> fst p ++ ": " ++ snd p ++ "; ")
        |> String.concat
        |> Svg.Attributes.style

-- The view of a label

view : String -> Float -> Float -> Float -> Float -> Svg
view label x y width opacity =
    let
        labelCharWidth = width |> pixelsToChars
        spltScale = splitAndScale label labelCharWidth
        splt = fst spltScale
        fontPercent = snd spltScale |> toPercent
        textColour = pickTextColour label
        labelLine txt dy =
            Svg.text'
            [ styles
              [ ("fill", textColour)
              , ("font-size", fontPercent)
              , ("opacity", opacity |> toString)
              ]
            , Svg.Attributes.x (x |> toString)
            , Svg.Attributes.y (y + dy |> toString)
            , Svg.Attributes.textAnchor "middle"
            ]
            [ Svg.text txt ]
    in
        case splt of
            Whole txt -> labelLine txt 0
            Halves (txt1, txt2) ->
                Svg.g
                []
                [ labelLine txt1 0
                , labelLine txt2 10
                ]

