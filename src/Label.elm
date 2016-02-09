module Label
    ( pixelsToChars
    , Split(Halves, Whole), split
    , Part(Block, Whitespace), toParts
    ) where

import String exposing (cons, uncons, fromChar, left, length, right, dropRight, trimRight, dropLeft, trimLeft)

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

----------------------------------------------------------

-- Break some text into blocks which may wrap

toParts : String -> List Part
toParts text =
    toParts' text []
        |> doubleReverse

toParts' : String -> List Part -> List Part
toParts' rest accumChars =
    case uncons rest of
        Just (hd, tl) ->
            toParts' tl (addChar hd accumChars)
        Nothing ->
            -- rest was an empty string
            accumChars

-- Revers a List Part, and all the strings in each one
-- So [ Block "one", Whitespace " "] becomes [ Whitespace " ", Block "eno" ]

doubleReverse : List Part -> List Part
doubleReverse parts =
    List.reverse parts
        |> List.map reversePart

reversePart : Part -> Part
reversePart part =
    case part of
        Block text ->
            String.reverse text |> Block
        Whitespace text ->
            String.reverse text |> Whitespace

-- Add a character onto the start of a list of parts.
-- If it's of the same type as the head then it goes into that part
-- otherwise it's a new part.

addChar : Char -> List Part -> List Part
addChar next accumChars =
    let
        charPart = charToPart next
    in
        case accumChars of
            [] ->
                [ toPartString charPart ]
            headPart :: tailParts ->
                consPart charPart headPart tailParts

-- Describe a Char as a Part

charToPart : Char -> PartChar
charToPart chr =
    if (isWhitespace chr) then
        WhitespaceChar chr
    else
        BlockChar chr

-- Is a character whitespace?

isWhitespace : Char -> Bool
isWhitespace chr =
    (chr == ' ')
    || (chr == '\t')
    || (chr == '\n')
    || (chr == '\r')

-- Get a Part (for a string) from a PartChar

toPartString : PartChar -> Part
toPartString partChr =
    case partChr of
        BlockChar chr -> Block (fromChar chr)
        WhitespaceChar chr -> Whitespace (fromChar chr)

-- Put a PartChar onto the front of a List Part, which has already
-- been separated into a head and tail.

consPart : PartChar -> Part -> List Part -> List Part
consPart chrPart headPart tailParts =
    case (chrPart, headPart) of
        (BlockChar chr, Block head) ->
            consBlockToBlock chr head tailParts
        (WhitespaceChar chr, Block head) ->
            (toPartString chrPart) :: headPart :: tailParts
        (BlockChar chr, Whitespace head) ->
            (toPartString chrPart) :: headPart :: tailParts
        (WhitespaceChar chr, Whitespace head) ->
            (cons chr head |> Whitespace) :: tailParts

-- Add a char (from a Block) onto a string (from a Block)
-- and put them onto the front of a List Part.
-- Ordinarily that's trivial, but if the string has just
-- had a hyphen added to it then we need to create a new block
-- for the char.

consBlockToBlock : Char -> String -> List Part -> List Part
consBlockToBlock chr str parts =
    case uncons str of
        Just ('-', rest) ->
            Block (fromChar chr) :: Block str :: parts
        _ ->
            (cons chr str |> Block) :: parts


