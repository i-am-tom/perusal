module Perusal.Animation where

import Data.Functor (map)
import Data.Ring ((-))
import FRP.Event (Event)
import FRP.Event.Time (withTime)
import FRP.Behavior (sample)
import FRP.Behavior.Time (millisSinceEpoch)

withDelay :: forall a. Event a -> Event { delay :: Number, value :: a }
withDelay event = sample millisSinceEpoch (map go (withTime event))
  where go { time, value } now = { delay: now - time, value }
