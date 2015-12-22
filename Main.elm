import BubblesHtml exposing (update, view, Action)

import StartApp.Simple exposing (start)

bubble1Model = { x = 340, y = 200, size = 180, colour = "red" }
bubble2Model = { x = 450, y = 250, size = 100, colour = "green" }

model = { width = 800, height = 600, bubbles = [bubble1Model, bubble2Model] }

main =
    start
        { model = model
        , update = update
        , view = view
        }

