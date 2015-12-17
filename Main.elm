import Bubbles exposing (update, view)
import StartApp.Simple exposing (start)

main =
        start
                { model = { x = 0, y = 0 }
                , update = update
                , view = view
                }


