import BubblesHtml exposing (update, view)
import StartApp.Simple exposing (start)

bubblesModel = { x = 0, y = 0 }

main =
    start
        { model = { width = 300, height = 300, bubbles = bubblesModel }
        , update = update
        , view = view
        }


