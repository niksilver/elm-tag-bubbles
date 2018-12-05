module Out exposing (Msg(..), takeFirstActualMsg)

import Status
import Help
import Constants


--
-- Messages to out to the top level
--


type Msg
  = StatusMsg Status.Msg
  | HelpMsg Help.Help
  | RequestTag Constants.Tag
  | Recentre
  | None


takeFirstActualMsg : List Msg -> Msg
takeFirstActualMsg msgs =
  case List.head msgs of
    Just None ->
      case List.tail msgs of
        Just tail ->
          takeFirstActualMsg tail
        Nothing ->
          None

    Just actualMsg ->
      actualMsg

    Nothing ->
      None

