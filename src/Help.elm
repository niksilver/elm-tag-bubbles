module Help exposing (Help(..), view)


import Html exposing (Html, div, text, h1, p, button, a)
import Html.Attributes exposing (id, class, href)
import Html.Events exposing (onClick)


type Help = On | Off


helpText : List String
helpText = [ """The Guardian produces a lot of content every day, and each
piece of content covers several different topics. How are these topics related?
This app helps us visualise that.
"""
  , """Each bubble represents a topic.
Click a bubble to find out more about it. The application
will fetch the latest content on that topic, and will discover all
the other topics covered by that same content. The larger the bubble
the more that topic has been covered within the one you chose.
The closer two bubbles are
the more content covers both of those topics."""

  , """On the bottom left you will see if the app is ready for your next
click or if it is currently fetching the latest articles on
your chosen topic."""

  , """If you can't read the topic name in a bubble then simply mouse over
the bubble and its name will appear on the bottom right."""
  ]


loveHtml : List (Html x)
loveHtml =
  [ text "Made with "
  , a
    [ href "http://niksilver.com/2016/03/08/discovering-the-elm-language/" ]
    [ text "love and sweat" ]
  , text " in "
  , a
    [ href "http://elm-lang.org/" ]
    [ text "the Elm language" ]
  ]


sourceHtml : List (Html x)
sourceHtml =
  [ text "Source on "
  , a
    [ href "https://github.com/niksilver/elm-tag-bubbles" ]
    [ text "GitHub" ]
  ]


helpHtml : List (Html a)
helpHtml =
  helpText
  |> List.map text
  |> List.map (\t -> p [] [t])


helpDiv : Html Help
helpDiv =
  let
      heading = h1 [] [ text "Tag bubbles" ]
      done =
        div [ id "buttonDiv" ]
        [ button [ onClick Off ] [ text "Done" ] ]
      content = List.concat [ [heading], helpHtml, [done] ]
  in
      div [ id "help" ]
      [ div [ id "helpContent" ] content
      , div [ id "helpLove", class "footer" ] loveHtml
      , div [ id "helpSource", class "footer" ] sourceHtml
      ]


view : Help -> Html Help
view help =
  if (help == On) then
    helpDiv
  else
    text ""


