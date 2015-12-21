import Bubbles
import BubblesHtml exposing (update, view, Action)

import StartApp.Simple exposing (start)

bubblesModel = { x = 40, y = 0 }

model = { width = 300, height = 300, bubbles = bubblesModel }

main =
    start
        { model = model
        , update = update
        , view = view
        }

