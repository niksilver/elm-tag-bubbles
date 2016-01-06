module Context (Context, forwardTo) where

import Signal

type alias Context a =
    { click : Signal.Address a, address : Signal.Address a }

forwardTo : Context b -> (a -> b) -> Context a
forwardTo context fn =
    { context
    | click = Signal.forwardTo context.click fn
    , address = Signal.forwardTo context.address fn
    }
