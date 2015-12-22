import BubblesHtml exposing (update, view)

import StartApp.Simple exposing (start)

bubble1Model = { x = 340, y = 200, size = 180, colour = "red" }
bubble2Model = { x = 450, y = 250, size = 100, colour = "green" }
bubble3Model = { x = 480, y = 350, size = 80, colour = "red" }
bubble4Model = { x = 400, y = 400, size = 60, colour = "green" }

model = { width = 800
    , height = 600
    , bubbles =
        [bubble1Model
        , bubble2Model
        , bubble3Model
        , bubble4Model
        ]
    }

main =
    start
        { model = model
        , update = update
        , view = view
        }

