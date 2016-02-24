module NavBar (view) where

import Constants
import Context exposing (Context)
import World
import Help exposing (Help(On))

import Html exposing (Html, button, text, div, input, label)
import Html.Attributes as Attrs exposing (style, type', class, for, id)
import Html.Events exposing (onClick, on, targetValue)


view : Context World.Action -> Context Help -> Float -> Html
view worldContext helpContext scale =
    let
        scaleMessage : String -> Signal.Message
        scaleMessage e =
            World.Scale e
                |> Signal.message worldContext.address
    in
        div
        [ id "navbar"
        , style
          [ ("height", toString Constants.navHeight ++ "px")
          , ("text-align", "center")
          ]
        ]
        [ div
          -- Vertical alignment trick from
          -- http://zerosixthree.se/vertical-align-anything-with-just-3-lines-of-css/
          [ class "vcentre" ]
          [ button
            [ onClick worldContext.address World.Recentre ]
            [ text "Recentre" ]
          , label
            [ for "scale" ]
            [ text "Scale" ]
          , input
            [ type' "range"
            , Attrs.min "0.2"
            , Attrs.max "2.5"
            , Attrs.value (toString scale)
            , Attrs.step "0.01"
            , id "scale"
            -- Need both onChange and onInput according to
            -- stackoverflow.com/questions/18544890
            , on "change" targetValue scaleMessage
            , on "input" targetValue scaleMessage
            ]
            []
          , button
            [ onClick helpContext.address Help.On ]
            [ text "Help" ]
          ]
        ]
