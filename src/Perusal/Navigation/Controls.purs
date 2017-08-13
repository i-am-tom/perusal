module Perusal.Navigation.Controls where

import Prelude hiding (when)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.MonadZero (guard)
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Set (Set, intersection, isEmpty, member)
import Data.Tuple (Tuple(Tuple), snd)
import FRP (FRP)
import FRP.Event (Event, create, filter, fold, mapMaybe, sampleOn_, withLast)
import FRP.Event.Keyboard (down)
import FRP.Event.Time (withTime)


-- | Supa-simple data type to generalise `fromInput` to the choice of
-- | "left" or "right". Yes, it's isomorphic to `Boolean`, but I'm
-- | gonna put code clarity above minor code optimisation.
data Direction = Prev | Next


-- | Calculate whether two sets overlap. This isn't that exciting.
overlaps :: forall a. Ord a => Set a -> Set a -> Boolean
overlaps x y = not isEmpty $ x `intersection` y


-- | This little rascal tells us if any directional keys are being
-- | pressed, and, if so, whether they correspond to `Next` or `Prev`.
-- | Note the `MonadReader m` - we query the "global config" for our
-- | key sets here.
fromInput :: forall m r
           . MonadReader { prev :: Set Int, next :: Set Int | r } m
          => m (Event Direction)
fromInput = ask <#> \{ prev, next } -> mapMaybe (go prev next) down
  where

    go prev next key
      | key `member` prev = Just Prev
      | key `member` next = Just Next
      | otherwise         = Nothing


-- | Chances are that you _don't_ want a `Direction`, and you're
-- | really more interested in another type. Well, now's your
-- | chance to shine. *Go get 'em, tiger*.
fromDirection :: forall a. a -> a -> Direction -> a
fromDirection prev next = case _ of Prev -> prev
                                    Next -> next


-- | Here's an interesting problem to solve: you have an event being
-- | sampled according to the tick of a second stream. You get results
-- | back, but you have no idea _when_ they happened; with this little
-- | menace, we can solve that little problem.
withDelay :: forall a b. Event b -> Event a -> Event (Tuple Int a)
withDelay sampler e = go <$> withTime (sampleOn_ (withTime e) sampler)
  where

    -- Calculate a duration!
    go :: { time :: Int, value :: { time :: Int, value :: a } }
       -> Tuple Int a
    go { time: now, value: { time: start, value } } =
      Tuple (now - start) value


-- | Filter out events from a stream that occur while a `Boolean` behavior is
-- | `false`.
when :: forall a. Event Boolean -> Event a -> Event a
when ps xs = mapMaybe id $ (\p x -> guard p $> x) <$> ps <*> xs


-- | Map over an event, but carry an accumulator value with you. This
-- | can be pretty useful if, for example, you want to attach IDs to
-- | events: `mapAccum (\x i -> Tuple (i + 1) (Tuple x i)) 0`.
mapAccum :: forall a b c
          . (a -> b -> Tuple b c)
         -> Event a
         -> b
         -> Event c
mapAccum f xs acc = mapMaybe snd
  $ fold (\a (Tuple b _) -> pure <$> f a b) xs
  $ Tuple acc Nothing


-- | Create an event that can block itself. This is pretty exciting if
-- | you want to, say, disable key presses during an animation...
withBlocking :: forall a eff m
              . MonadEff (frp :: FRP | eff) m
             => Event a
             -> m { event :: Event a
                  , push  :: Boolean -> Eff (frp :: FRP | eff) Unit
                  }
withBlocking action = liftEff create <#>
  \ks -> ks { event = when ks.event action }


-- | When you use `withLast` on a `Maybe` stream, you probably don't
-- | get the desired behaviour. _This_ function is `withLast`, but will
-- | give you the `last` that was a `Just`!
withLastJust :: forall a
              . Event (Maybe a)
             -> Event (Maybe { now :: a, last :: Maybe a })
withLastJust e = (filter isNothing e $> Nothing)
  <|> (pure <$> withLast (mapMaybe id e))
