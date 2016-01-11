module Context
    ( Context
    , CountedClick (NoClick, SingleClick, DoubleClick)
    , mappedCountedClicks
    , create, forwardTo
    ) where

-- Mechanism for capturing not only messages to a mailbox address,
-- but also clicks (with an identifier) which might become double clicks.

import Signal exposing (Address, foldp)
import Time exposing (Time)

-- A mouse click with a String identifier

type CountedClick = NoClick | SingleClick String | DoubleClick String

-- Addresses for click identifiers and other messages

type alias Context a =
    { click : Address String
    , address : Signal.Address a
    }

-- A mailbox that collects id'd clicks, which might turn into double clicks.
-- The clicks go like this:
-- address for Signal String
--   mapping to ==>
-- Signal (Time, String)
--   forwarded to ==>
-- address for (Time, SingleClick/DoubleClick String)
--   mapping to ==>
-- Signal SingleClick/DoubleClick String
--   mapping to ==>
-- application-specific wrapping of SingleClick/DoubleClick String

clickBox : Signal.Mailbox String
clickBox = Signal.mailbox "n/a"

timedId : Signal (Time, String)
timedId = Time.timestamp clickBox.signal

toCountedClick : (Time, String) -> (Time, CountedClick) -> (Time, CountedClick)
toCountedClick timedString timedClick =
    let
        time1 = fst timedString
        time2 = fst timedClick
        tag1 = snd timedString
        click2 = snd timedClick
        quickClicks = (time1 - time2 < 500 * Time.millisecond)
    in
        case click2 of
            NoClick ->
                (time1, SingleClick tag1)
            DoubleClick _ ->
                (time1, SingleClick tag1)
            SingleClick tag2 ->
                if (tag1 == tag2 && quickClicks) then
                    (time2, DoubleClick tag1)
                else
                    (time1, SingleClick tag1)

timedCountedClicks : Signal (Time, CountedClick)
timedCountedClicks =
    foldp toCountedClick (0, NoClick) timedId

countedClicks : Signal CountedClick
countedClicks = Signal.map snd timedCountedClicks

mappedCountedClicks : (CountedClick -> a) -> Signal a
mappedCountedClicks f =
    Signal.map f countedClicks

-- Create a `Context` for which the click address and main address.
-- both to a given address. But the first two parameters say
-- how to map an incoming signal from another type to the target type.
-- The first argument is the map for the click address; the second
-- for the main address.

create : Signal.Address a -> Context a
create address =
    { click = clickBox.address
    , address = address
    }

-- Create a context with forwarding addresses, using the mapping specified

forwardTo : Context b -> (a -> b) -> Context a
forwardTo context fn =
    { context
    | address = Signal.forwardTo context.address fn
    }

