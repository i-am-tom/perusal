module Perusal.Navigation (movement) where

import Prelude (type (~>), otherwise)

import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Set (member, Set)
import Data.Tape (Tape, left, right)

import FRP.Behavior (Behavior, step, sample_)
import FRP.Behavior.Keyboard (keys)
import FRP.Event (Event, fold)
import FRP.Event.Keyboard (down)

keysDown :: Event (Set Int)
keysDown = sample_ keys down

movement :: forall a. Tape a -> Behavior (Tape a)
movement deck = step deck (fold go keysDown deck)

  where next :: forall b. Set Int -> Tape b -> Maybe (Tape b)
        next keys | 37 `member` keys = left
                  | 39 `member` keys = right
                  | otherwise        = Just

        go :: Set Int -> Tape ~> Tape
        go keys tape = fromMaybe tape (next keys tape)

-- state :: forall a. Tape a -> Behavior (Tape a)
