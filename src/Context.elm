module Context (Context, create, forwardTo) where

import Signal

type alias Context a =
    { click : Signal.Address a, address : Signal.Address a }

type Message = Message String

-- Create a `Context` for which the click address and main address
-- both to a given address. But the first two parameters say
-- how to map an incoming signal from another type to the target type.
-- The first argument is the map for the click address; the second
-- for the main address.

create : (a -> b) -> (a -> b) -> Signal.Address b -> Context a
create clickMap addressMap address =
    { click = Signal.forwardTo address clickMap
    , address = Signal.forwardTo address addressMap
    }

-- Create a context with forwarding addresses, using the mapping specified

forwardTo : Context b -> (a -> b) -> Context a
forwardTo context fn =
    { context
    | click = Signal.forwardTo context.click fn
    , address = Signal.forwardTo context.address fn
    }

