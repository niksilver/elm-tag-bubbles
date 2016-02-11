module Context
    ( Context
    , CountedClick (NoClick, SingleClick, DoubleClick)
    , mappedCountedClicks
    , create, forwardTo, forwardStatus
    ) where

-- Mechanism for capturing not only messages to a mailbox address,
-- but also clicks (with an identifier) which might become double clicks.

import Constants exposing (Tag)
import Status exposing (Status)

import Signal exposing (Address, foldp)
import Time exposing (Time)

-- A mouse click with a Tag identifier

type CountedClick = NoClick | SingleClick Tag | DoubleClick Tag

-- Addresses for click identifiers and other messages

type alias Context a =
    { click : Address Tag
    , status : Address Status.Action
    , address :Address a
    }

-- A mailbox that collects id'd clicks, which might turn into double clicks.
-- The clicks go like this:
-- address for Signal Tag
--   mapping to ==>
-- Signal (Time, Tag)
--   forwarded to ==>
-- address for (Time, SingleClick/DoubleClick Tag)
--   mapping to ==>
-- Signal SingleClick/DoubleClick Tag
--   mapping to ==>
-- application-specific wrapping of SingleClick/DoubleClick Tag

clickBox : Signal.Mailbox Tag
clickBox = Signal.mailbox (Tag "nullid" "No-op tag")

timedId : Signal (Time, Tag)
timedId = Time.timestamp clickBox.signal

toCountedClick : (Time, Tag) -> (Time, CountedClick) -> (Time, CountedClick)
toCountedClick timedTag timedClick =
    let
        time1 = fst timedTag
        time2 = fst timedClick
        tag1 = snd timedTag
        click2 = snd timedClick
        quickClicks = (time1 - time2 < 200 * Time.millisecond)
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

-- A mailbox for status updates

statusBox : Signal.Mailbox Status.Action
statusBox = Signal.mailbox (Status.NoOverlay)

-- Create a `Context` for sending event data.
-- The argument is the main address.

create : Signal.Address a -> Context a
create address =
    { click = clickBox.address
    , status = statusBox.address
    , address = address
    }

-- Create a context with forwarding addresses, using the mapping specified

forwardTo : Context b -> (a -> b) -> Context a
forwardTo context fn =
    { context
    | address = Signal.forwardTo context.address fn
    }

-- Create a context in which the status actions are forwarded to
-- the main address with a given mapping

forwardStatus : (Status.Action -> b) -> Context b -> Context b
forwardStatus fn context =
    { context
    | status = Signal.forwardTo context.address fn
    }

