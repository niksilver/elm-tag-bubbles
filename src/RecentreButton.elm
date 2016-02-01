module RecentreButton (view) where

import Context exposing (Context)
import World

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)

view : Context World.Action -> (Int, Int) -> Html
view context dimensions =
    button
    [ onClick context.address (World.Recentre dimensions)
    ]
    [ text "Recentre"
    ]
