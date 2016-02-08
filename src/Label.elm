module Label
    ( pixelsToChars
    , Part(Block, Whitespace), toParts
    ) where

import String exposing (cons, uncons, fromChar)

-- Breaking text into parts suitable for line breaks

type Part = Block String | Whitespace String

type PartChar = BlockChar Char | WhitespaceChar Char

-- How many characters can we fit into a pixel width?

pixelsToChars : Float -> Int
pixelsToChars px =
    px / 16 * 1.8  -- 1em = 16px, and the average character is narrower then 1em
        |> floor

-- Break some text into blocks which may wrap

toParts : String -> List Part
toParts text =
    toParts' text []
        |> doubleReverse

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

toParts' : String -> List Part -> List Part
toParts' rest accumChars =
    case uncons rest of
        Just (hd, tl) ->
            toParts' tl (addChar hd accumChars)
        Nothing ->
            -- rest was an empty string
            accumChars

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

charToPart : Char -> PartChar
charToPart chr =
    if (chr == ' ') then
        WhitespaceChar chr
    else
        BlockChar chr

toPartString : PartChar -> Part
toPartString partChr =
    case partChr of
        BlockChar chr -> Block (fromChar chr)
        WhitespaceChar chr -> Whitespace (fromChar chr)

consPart : PartChar -> Part -> List Part -> List Part
consPart chrPart headPart tailParts =
    case (chrPart, headPart) of
        (BlockChar chr, Block head) ->
            (cons chr head |> Block) :: tailParts
        (WhitespaceChar chr, Block head) ->
            (toPartString chrPart) :: headPart :: tailParts
        (BlockChar chr, Whitespace head) ->
            (toPartString chrPart) :: headPart :: tailParts
        (WhitespaceChar chr, Whitespace head) ->
            (cons chr head |> Whitespace) :: tailParts

