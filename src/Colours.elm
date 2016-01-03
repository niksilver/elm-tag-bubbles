module Colours
    ( colours
    , colourNames, colourBases, colourText
    , greys
    , greyNames, greyBases, greyText
    , pickBaseColour, pickTextColour
    ) where

import Color exposing (Color, rgb, black, white, toRgb)
import List exposing (map)
import Array exposing (Array)
import Maybe exposing (withDefault)
import String
import Char

type alias Colour = { name : String, base : Color, text : Color }

colList : List Colour
colList =
    [ Colour "Butter 1" (rgb 252 233 79) black
    , Colour "Butter 2" (rgb 237 212 0) black
    , Colour "Butter 3" (rgb 196 160 0) white
    , Colour "Chameleon 1" (rgb 138 226 52) black
    , Colour "Chameleon 2" (rgb 115 210 22) black
    , Colour "Chameleon 3" (rgb 78 154 6) white
    , Colour "Orange 1" (rgb 252 175 62) black
    , Colour "Orange 2" (rgb 245 121 0) white
    , Colour "Orange 3" (rgb 206 92 0) white
    , Colour "Sky Blue 1" (rgb 114 159 207) black
    , Colour "Sky Blue 2" (rgb 52 101 164) white
    , Colour "Sky Blue 3" (rgb 32 74 135) white
    , Colour "Plum 1" (rgb 173 127 168) white
    , Colour "Plum 2" (rgb 117 80 123) white
    , Colour "Plum 3" (rgb 92 53 102) white
    , Colour "Chocolate 1" (rgb 233 185 110) black
    , Colour "Chocolate 2" (rgb 193 125 17) white
    , Colour "Chocolate 3" (rgb 143 89 2) white
    , Colour "Scarlet Red 1" (rgb 239 41 41) white
    , Colour "Scarlet Red 2" (rgb 204 0 0) white
    , Colour "Scarlet Red 3" (rgb 164 0 0) white
    ]

greyList : List Colour
greyList =
    [ Colour "Aluminium 1" (rgb 238 238 236) black
    , Colour "Aluminium 2" (rgb 211 215 207) black
    , Colour "Aluminium 3" (rgb 186 189 182) black
    , Colour "Aluminium 4" (rgb 136 138 133) white
    , Colour "Aluminium 5" (rgb 85 87 83) white
    , Colour "Aluminium 6" (rgb 46 52 54) white
    ]


{-| All the (non-grey) colours. -}
colours : Array Colour
colours = Array.fromList colList

{-| Names of all the (non-grey) colours. -}
colourNames : Array String
colourNames =
    map .name colList |> Array.fromList

{-| Actual colour of all the (non-grey) colours. -}
colourBases : Array Color
colourBases =
    map .base colList |> Array.fromList

{-| Text colour for each of the (non-grey) colours. -}
colourText : Array Color
colourText =
    map .text colList |> Array.fromList


{-| All the shades of grey. -}
greys : Array Colour
greys = Array.fromList greyList

{-| Names of the shades of grey. -}
greyNames : Array String
greyNames =
    map .name greyList |> Array.fromList

{-| Actual colour of the shades of grey. -}
greyBases : Array Color
greyBases =
    map .base greyList |> Array.fromList

{-| Text colour for each shade of grey. -}
greyText : Array Color
greyText =
    map .text greyList |> Array.fromList

{-| Pick some colour index for a given string. -}
pickColourIndex : String -> Int
pickColourIndex s =
    String.toList s
        |> map Char.toCode
        |> List.sum
        |> (\i -> i % (Array.length colours))

{-| Pick a base colour (as a W3C string) for a given string. -}
pickBaseColour : String -> String
pickBaseColour s =
    Array.get (pickColourIndex s) colourBases
        |> withDefault (rgb 60 60 60)
        |> toW3C

{-| Pick a suitable text colour (as a W3C string) for a given string. -}
pickTextColour : String -> String
pickTextColour s =
    Array.get (pickColourIndex s) colourText
        |> withDefault white
        |> toW3C

{-| Turn a `Color` into a W3C-friendly string. E.g. `"rgb(34, 89, 109)"`. -}
toW3C : Color -> String
toW3C col =
    let
        rgba = toRgb col
    in
        "rgb(" ++ (toString rgba.red) ++
        "," ++ (toString rgba.green) ++
        "," ++ (toString rgba.blue) ++
        ")"

