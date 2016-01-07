module Context
    ( Context
    , Clicker, toClicker, fromClicker
    , create, forwardTo
    ) where

-- Mechanism for capturing not only messages to a mailbox address,
-- but also clicks leading to effects, and which might become double clicks.

import Signal

type alias Context a =
    { click : Signal.Address Clicker, address : Signal.Address a }

-- A mouse click with a tag attached.

type Clicker = Clicker String

toClicker : String -> Clicker
toClicker tag =
    Clicker tag

fromClicker : Clicker -> String
fromClicker clicker =
    case clicker of Clicker tag -> tag

-- Create a `Context` for which the click address and main address
-- both to a given address. But the first two parameters say
-- how to map an incoming signal from another type to the target type.
-- The first argument is the map for the click address; the second
-- for the main address.

create : (Clicker -> b) -> (a -> b) -> Signal.Address b -> Context a
create clickMap addressMap address =
    { click = Signal.forwardTo address clickMap
    , address = Signal.forwardTo address addressMap
    }

-- Create a context with forwarding addresses, using the mapping specified

forwardTo : Context b -> (a -> b) -> Context a
forwardTo context fn =
    { context
    | address = Signal.forwardTo context.address fn
    }

