module Perusal.Navigation.Controls where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader.Class (class MonadReader, ask)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (Set, intersection, isEmpty, member)
import Data.Tuple (Tuple, fst, snd)
import FRP (FRP)
import FRP.Event (Event, create, mapMaybe, sampleOn, sampleOn_, subscribe)
import FRP.Event.Keyboard (down)
import FRP.Event.Time (withTime)
import Prelude hiding (when)


data Direction
  = Prev
  | Next


overlaps
  :: forall a
   . Ord a
  => Set a
  -> Set a
  -> Boolean
overlaps x y
  = not isEmpty
  $ x `intersection` y


fromInput
  :: forall m r
   . MonadReader
       { prev :: Set Int
       , next :: Set Int
       | r
       } m
  => m (Event Direction)
fromInput
  = ask <#> \{ next, prev } -> mapMaybe (go prev next) down
  where

    go :: Set Int -> Set Int -> Int -> Maybe Direction
    go prev next key
      | key `member` prev = Just Prev
      | key `member` next = Just Next
      | otherwise         = Nothing


fromDirection
  :: forall a
   . a
  -> a
  -> Direction
  -> a
fromDirection prev next
  = case _ of
      Prev -> prev
      Next -> next


sampleIf
  :: forall p x
   . (p -> x -> Boolean)
  -> Event p
  -> Event x
  -> Event x
sampleIf f s
  = mapMaybe id
  <<< sampleOn s
  <<< map \x p -> if f p x then Just x
                           else Nothing


withTime'
  :: forall a
   . Event a
  -> Event
       { time  :: Number
       , value :: a
       }
withTime'
  = map (\ev -> ev { time = toNumber ev.time })
  <<< withTime


withBlocking
  :: forall a b eff m
   . MonadEff (frp :: FRP | eff) m
  => Event a
  -> (Event a -> Event (Tuple Number b))
  -> m (Event b)
withBlocking event process
  = do
      blocker <- liftEff create

      let
        unblocked :: Event a
        unblocked
          =   _.value
          <$> sampleIf expired blocker.event stamped

        processed :: Event (Tuple Number b)
        processed = process unblocked

      liftEff $ subscribe (withTime' processed) \{ time, value } ->
        blocker.push (time + fst value)

      liftEff $ blocker.push 0.0

      pure $ snd <$> processed
  where

    stamped :: Event { time :: Number, value :: a }
    stamped = withTime' event

    expired :: Number
            -> { time :: Number, value :: a }
            -> Boolean
    expired expiry { time }
      = expiry < time


withDelay :: forall a b
           . Event a
          -> Event b
          -> Event { delay :: Number, value :: a }
withDelay event sampler
  = prepare <$> withTime' (sampleOn_ (withTime' event) sampler)
  where

    prepare
      :: { time :: Number
         , value :: { time :: Number, value :: a }
         }
      -> { delay :: Number, value :: a }
    prepare { time, value: { time: time', value } }
      = { delay: time - time', value }
