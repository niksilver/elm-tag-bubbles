module NavBar exposing (Msg, update, view)

import Constants
-- import Context exposing (Context)
-- import World
import Help exposing (Help)
import Out

import Html exposing (Html, button, text, div, input, label)
import Html.Attributes as Attrs exposing (style, type_, class, for, id)
import Html.Events exposing (onClick, on, targetValue)


type Msg
  = HelpMsg Help
  | Recentre
  | Scale String


-- We don't update any model, but we do convert in messages to out (top level) messages

update : Msg -> Out.Msg
update msg =
  case msg of
    HelpMsg helpMsg ->
      Out.HelpMsg helpMsg

    Recentre ->
      Out.Recentre

    Scale factorStr ->
      case String.toFloat factorStr of
        Just factor -> Out.Scale factor
        Nothing ->
          let
              _ = Debug.log "Couldn't parse scale factor string" factorStr
          in
              Out.None


view : Float -> Html Msg
view factor =
--     let
--         scaleMessage : String -> Signal.Message
--         scaleMessage e =
--             World.Scale e
--                 |> Signal.message worldContext.address
--     in
        div
        [ id "navbar"
        , style "height" (String.fromInt Constants.navHeight ++ "px")
        , style "text-align" "center"
        ]
        [ div
          -- Vertical alignment trick from
          -- http://zerosixthree.se/vertical-align-anything-with-just-3-lines-of-css/
          [ class "vcentre" ]
          [ button
            [ onClick Recentre ]
            [ text "Recentre" ]
          , label
            [ for "scale"
            , id "scale-label"
            ]
            [ text "Scale" ]
          , input
            [ type_ "range"
            , Attrs.min "0.2"
            , Attrs.max "2.5"
            , Attrs.value (String.fromFloat factor)
            , Attrs.step "0.01"
            , id "scale"
            -- Need both onChange and onInput according to
            -- stackoverflow.com/questions/18544890
            , on "change" targetValue |> Attrs.map Scale
            , on "input" targetValue |> Attrs.map Scale
            ]
            []
          , button
            [ onClick (HelpMsg Help.On) ]
            [ text "Help" ]
          ]
        ]
