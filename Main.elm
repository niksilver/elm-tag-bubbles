import Bubble
import BubblesHtml exposing (update, view, Action)

import StartApp.Simple exposing (start)

bubbleModel = { x = 40, y = 0, size = 180 }

model = { width = 300, height = 300, bubble = bubbleModel }

main =
    start
        { model = model
        , update = update
        , view = view
        }

