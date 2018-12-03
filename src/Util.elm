module Util exposing
  ( takeFirstJust
  , pairWith
  )


-- Take the first element of a list that's `Just` something

takeFirstJust : List (Maybe a) -> Maybe a
takeFirstJust list =
  case List.head list of
    Just Nothing ->
      case List.tail list of
        Just tail ->
          takeFirstJust tail
        Nothing ->
          Nothing

    Just elt ->
      elt

    Nothing ->
      Nothing

-- Add a second value to create a pair

pairWith : b -> a -> (a, b)
pairWith b a =
  (a, b)


