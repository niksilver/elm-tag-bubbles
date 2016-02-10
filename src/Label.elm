module Label
    ( pixelsToChars
    , Split(Halves, Whole), split
    , leastLength
    ) where

import String exposing
    ( cons, uncons, fromChar, left, length, right
    , dropRight, trimRight, dropLeft, trimLeft
    )

-- Splitting a string into halves (if possible)

type Split = Halves (String, String) | Whole String

-- Breaking text into parts suitable for line breaks

type Part = Block String | Whitespace String

type PartChar = BlockChar Char | WhitespaceChar Char

-- How many characters can we fit into a pixel width?

pixelsToChars : Float -> Int
pixelsToChars px =
    px / 16 * 1.8  -- 1em = 16px, and the average character is narrower then 1em
        |> floor

-- Split a string (if possible)

split : String -> Split
split text =
    let
        halfIdx = length text // 2
        front = left halfIdx text
        half1 = backSplitFrom front
        remainder = dropLeft (length half1) text
        half2 = trimLeft remainder
    in
        if (half1 /= "") then
            Halves (half1, half2)
        else
            splitForward text
                |> toType

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

-- Least length a label can be (in characters) given it may be split

leastLength : String -> Int
leastLength text =
    case split text of
        Halves (a, b) -> max (length a) (length b)
        Whole a -> length a

