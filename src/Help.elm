module Help (view) where

import Html exposing (Html, div, text, h1, p)
import Html.Attributes exposing (id)

helpText : List String
helpText = [ """The Guardian produces lots of articles every day, and each
article covers several different topics. How are these topics related?
This app helps us visualise that.
"""
    , """Each bubble represents a topic.
Double-click a bubble to find out more about that topic. The application
will fetch the latest articles on that topic, and will discover all
the other topics that those articles also cover. The larger the bubble
the more that topic has been covered within the topic you chose.
The closer two bubbles are
the more those topics are written about together."""

    , """On the bottom left you will see if the app is ready for your next
double-click or if it is currently fetching the latest articles on
your chosen topic."""

    , """If you can't read the topic name in a bubble then simply mouse over
the bubble and its name will appear on the bottom right."""
    ]

helpHtml : List Html
helpHtml =
    helpText
        |> List.map text
        |> List.map (\t -> p [] [t])

view : Html
view =
    div [ id "help" ]
    [ div [ id "helpContent" ]
      (( h1 [] [ text "Tag bubbles" ] ) :: helpHtml)
    ]

