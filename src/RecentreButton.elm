module RecentreButton (view) where

import Context exposing (Context)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)

view : Context a -> Html
view context =
    button
    [ onClick context.recentre ()
    ]
    [ text "Recentre"
    ]
