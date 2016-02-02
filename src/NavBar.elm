module NavBar (view) where

import Constants
import Context exposing (Context)
import World

import Html exposing (Html, button, text, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

view : Context World.Action -> (Int, Int) -> Html
view context dimensions =
    div
    [ style
      [ ("height", toString Constants.navHeight ++ "px")
      , ("text-align", "center")
      ]
    ]
    [ button
      [ onClick context.address (World.Recentre dimensions)
      -- Vertical alignment trick from
      -- http://zerosixthree.se/vertical-align-anything-with-just-3-lines-of-css/
      , style
          [ ("position", "relative")
          , ("top", "50%")
          , ("transform", "translateY(-50%)")
          ]
      ]
      [ text "Recentre" ]
    ]
