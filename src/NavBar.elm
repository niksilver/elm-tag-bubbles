module NavBar (view) where

import Constants
import Context exposing (Context)
import World

import Html exposing (Html, button, text, div, input)
import Html.Attributes as Attrs exposing (style, type')
import Html.Events exposing (onClick, on, targetValue)


view : Context World.Action -> Float -> Html
view context scale =
    let
        scaleMessage : String -> Signal.Message
        scaleMessage e =
            World.Scale e
                |> Signal.message context.address
    in
        div
        [ style
          [ ("height", toString Constants.navHeight ++ "px")
          , ("text-align", "center")
          ]
        ]
        [ button
          [ onClick context.address World.Recentre
          -- Vertical alignment trick from
          -- http://zerosixthree.se/vertical-align-anything-with-just-3-lines-of-css/
          , style
              [ ("position", "relative")
              , ("top", "50%")
              , ("transform", "translateY(-50%)")
              ]
          ]
          [ text "Recentre" ]
        , input
          [ type' "range"
          , Attrs.min "0.2"
          , Attrs.max "2.5"
          , Attrs.value (toString scale)
          , Attrs.step "0.01"
          -- Need both onChange and onInput according to
          -- stackoverflow.com/questions/18544890
          , on "change" targetValue scaleMessage
          , on "input" targetValue scaleMessage
          ]
          []
        ]
