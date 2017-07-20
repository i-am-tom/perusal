module Perusal.Navigation (movement) where

import Prelude (type (~>), otherwise)

import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Set (member, Set)
import Data.Tape (Tape, left, right)

import FRP.Behavior (Behavior, step, sample_)
import FRP.Behavior.Keyboard (keys)
import FRP.Event (Event, fold)
import FRP.Event.Keyboard (down)

-- | An event, fired on every keydown, containing the currently-
-- | pressed keys.  We use these to find whether left or right have
-- | been pressed, and hence whether we should be moving
keysDown :: Event (Set Int)
keysDown = sample_ keys down

-- | A behaviour describing the progression of the user through the
-- | slide deck via presses of either left or right.
movement :: forall a. Tape a -> Behavior (Tape a)
movement deck = step deck (fold go keysDown deck)

        -- | Which way should we move? Left, right, or neither?
  where next :: forall b. Set Int -> Tape b -> Maybe (Tape b)
        next keys | 37 `member` keys = left
                  | 39 `member` keys = right
                  | otherwise        = Just

        -- | Based on the key presses, move the tape either left,
        -- | right, or in neither direction (depending on whether we
        -- | have reached either end of the tape or another key was
        -- | pressed).
        go :: Set Int -> Tape ~> Tape
        go keys tape = fromMaybe tape (next keys tape)
