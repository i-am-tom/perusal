module Perusal.Navigation.Controls where

import Prelude hiding (when)

import Control.Monad.Reader.Class (class MonadReader, ask)
import Data.Filterable (filterMap, filtered)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (Set, member)
import FRP.Behavior (sample)
import FRP.Behavior.Time (millisSinceEpoch)
import FRP.Event (Event, fix, sampleOn)
import FRP.Event.Keyboard (down)
import FRP.Event.Time (withTime)

-- | When you're dealing with a slideshow, there are two useful ways
-- to travel: forwards and backwards. This data type encompasses that
-- that *wealth* of information.
data Direction
  = Prev
  | Next

-- | If you can tell me what your "next" and "prev" keys are, I can
-- tell you (via this neat little event) which direction someone is
-- trying to select.
fromInput
  :: forall m r.
     MonadReader { prev :: Set Int, next :: Set Int | r } m
  => m (Event Direction)
fromInput
  = ask <#> \{ next, prev } ->
      let
        go :: Int -> Maybe Direction
        go key | key `member` prev = Just Prev
               | key `member` next = Just Next
               | otherwise         = Nothing

      in filterMap go down

-- | Directions are _fun_, but probably not what you're interested in.
-- This function takes a value for `Prev` and `Next`, and returns the
-- value that matches the given `Direction`. It's basically a
-- bastardised `if ... then ... else ...` expression.
fromDirection
  :: forall a.
     a
  -> a
  -> Direction
  -> a
fromDirection prev _    Prev = prev
fromDirection _    next Next = next

-- | Label an event with the time elapsed since its initial fire. We
-- can use this information for animations: with this number, we keep
-- track of our progress through a time period.
withDelay :: forall a.
             Event a
          -> Event { delay :: Number, value :: a }
withDelay
  = sample millisSinceEpoch
  <<< map prepare
  <<< withTime
  where

    -- Find the difference between "now" and "then".
    prepare :: { time  :: Number, value :: a }
            -> Number
            -> { delay :: Number, value :: a }
    prepare { time, value } now
      = { delay: now - time, value }

-- | Sample the events that are fired while a boolean event is true.
gate :: forall a. Event Boolean -> Event a -> Event a
gate = gateBy const

-- | Generalised form of `gateBy`, allowing for any predicate between
-- the two events. When true, the second event is sampled.
gateBy
  :: forall a b.
     (a -> b -> Boolean)
  -> Event a
  -> Event b
  -> Event b
gateBy f s
    = filtered
  <<< sampleOn s
  <<< map \x p -> if f p x then Just x else Nothing

-- | Provided a starting event and transformation, block the starter
-- event for the period described in the output.
withBlocking
  :: forall a b.
      Event a
  -> (Event a -> Event { period :: Number, value :: b })
  ->  Event b
withBlocking event process
  = fix \blocker ->
      let
        stamped :: Event { time :: Number, value :: a }
        stamped = withTime event

        comparison :: forall r. Number -> { time :: Number | r } -> Boolean
        comparison a b = a < b.time

        unblocked :: Event a
        unblocked = _.value <$> gateBy comparison blocker stamped

        processed :: Event { period :: Number, value :: b }
        processed = process unblocked

        withExpiry :: Event { expiry :: Number, value :: b }
        withExpiry = withTime processed <#>
          \{ time, value: { period,  value } } ->
            { expiry: time + period, value }
      in
        { input:  _.expiry <$> withExpiry
        , output: _.value  <$> withExpiry
        }
