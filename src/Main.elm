import UI
import UI.Setup
-- import UI.Tasks exposing (address)

-- import Constants exposing (TagsResult)

-- import Html exposing (Html)
import Browser
import Browser.Events
-- import Task exposing (Task)
-- import Signal exposing (Signal)


main =
  Browser.element
    { init = UI.Setup.init
    , update = UI.update
    , subscriptions = subscriptions
    , view = UI.view
    }


subscriptions : UI.Model -> Sub UI.Msg
subscriptions model =
  Browser.Events.onAnimationFrame UI.Tick


