import Bubbles
import BubblesHtml exposing (update, view, Action)

import Window
import Effects
import Signal
import StartApp exposing (start)

bubblesModel = { x = 40, y = 0 }

model = { width = 300, height = 300, bubbles = bubblesModel }

windowSigs : Signal Action
windowSigs = Signal.map BubblesHtml.Resize Window.dimensions

emptySigs : Signal Action
emptySigs = Signal.constant (BubblesHtml.Bubble Bubbles.Empty)

app =
    start
        { init = (model, Effects.none)
        , update = update
        , view = view
        , inputs = [ windowSigs, emptySigs ]
        }

main =
    app.html
