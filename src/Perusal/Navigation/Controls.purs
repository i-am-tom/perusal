module Perusal.Navigation.Controls where

import Prelude hiding (when)

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Set (Set, member)
import FRP.Event (Event)
import FRP.Event.Keyboard (down)

data Direction = Prev | Next

direct :: forall a. a -> a -> Direction -> a
direct left _     Prev = left
direct _    right Next = right

fromInput
  :: { next :: Set Int, prev :: Set Int }
  -> Event Direction
fromInput { next, prev } =
  let
    go key | key `member` prev = Just Prev
           | key `member` next = Just Next
           | otherwise         = Nothing
  in
    filterMap go down

