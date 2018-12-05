module Util exposing
  ( pairWith
  , httpErrorToString
  )


import Http


-- Add a second value to create a pair

pairWith : b -> a -> (a, b)
pairWith b a =
  (a, b)


-- Express an HTTP error as a string

httpErrorToString : Http.Error -> String
httpErrorToString error =
  case error of
    Http.BadUrl msg ->
      "Bad URL: " ++ msg

    Http.Timeout ->
      "Network timeout error"

    Http.NetworkError ->
      "Unknown network error"

    Http.BadStatus code ->
      "Error fetching data: status code " ++ (String.fromInt code)

    Http.BadBody msg ->
      "Error with data received: " ++ msg


