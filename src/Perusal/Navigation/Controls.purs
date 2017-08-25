module Perusal.Navigation.Controls where

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.MonadZero (guard)
import Data.Int (toNumber)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Set (Set, intersection, isEmpty, member)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))
import FRP (FRP)
import FRP.Event (Event, create, mapAccum, mapMaybe, sampleOn_)
import FRP.Event.Keyboard (down)
import FRP.Event.Time (withTime)
import Prelude hiding (when)


-- | Supa-simple data type to generalise `fromInput` to the choice of
-- | "left" or "right". Yes, it's isomorphic to `Boolean`, but I'm
-- | gonna put code clarity above minor code optimisation.
data Direction
  = Prev
  | Next


-- | Calculate whether two sets overlap. This isn't that exciting.
overlaps :: forall a.
            Ord a
         => Set a
         -> Set a
         -> Boolean
overlaps x y
  = not isEmpty (x `intersection` y)


-- | This little rascal tells us if any directional keys are being
-- | pressed, and, if so, whether they correspond to `Next` or `Prev`.
-- | Note the `MonadReader m` - we query the "global config" for our
-- | key sets here.
fromInput :: forall m r.
             MonadReader { prev :: Set Int
                         , next :: Set Int
                         | r
                         } m
          => m (Event Direction)
fromInput
  = ask <#> \{ next, prev }
      -> mapMaybe (go prev next) down

  where

    go prev next key
      | key `member` prev = Just Prev
      | key `member` next = Just Next
      | otherwise         = Nothing


-- | Chances are that you _don't_ want a `Direction`, and you're
-- | really more interested in another type. Well, now's your
-- | chance to shine. *Go get 'em, tiger*.
fromDirection :: forall a.
                 a
              -> a
              -> Direction
              -> a
fromDirection prev next
  = case _ of
      Prev -> prev
      Next -> next


-- | Here's an interesting problem to solve: you have an event being
-- | sampled according to the tick of a second stream. You get results
-- | back, but you have no idea _when_ they happened; with this little
-- | menace, we can solve that little problem.
withDelay :: forall a b.
             Event b
          -> Event a
          -> Event (Tuple Int a)
withDelay sampler event
  = go <$> withTime (sampleOn_ (withTime event) sampler)
  where

    -- Calculate a duration!
    go :: forall x.
          { time  :: Int
          , value :: { time  :: Int
                     , value :: x } }
       -> Tuple Int x
    go { time: now, value: { time: start, value } }
      = Tuple (now - start) value


-- | Filter out events from a stream that occur while a `Boolean`
-- | behavior is `false`.
when :: forall a.
        Event Boolean
     -> Event a
     -> Event a
when ps
  = sampleOn ps $ map \x p ->
      if p then Just x
           else Nothing

-- | When you use `withLast` on a `Maybe` stream, you probably don't
-- | get the desired behaviour. _This_ function is `withLast`, but
-- | will give you the `last` that was a `Just`!
withLastJust :: forall a.
                Event (Maybe a)
             -> Event (Maybe { now :: a, last :: Maybe a })
withLastJust e
  = mapAccum go e Nothing
  where

    go next last
      | Just now <- next = Tuple next (Just { now, last })
      | otherwise        = Tuple last Nothing


-- | Now we have the `withLastJust` helper, we can implement blocking
-- | by checking the last successful event's time and duration. If our
-- | current event's time is after that, we're good to go.
withBlocking :: forall r.
                Event { duration :: Milliseconds | r }
             -> m (Event { duration :: Milliseconds | r })
withBlocking
  = withLastJust <<< mapAccum go <<< prepare <<< withTime
  where

    prepare :: _
    prepare { time, value: value@{ duration } }
      = { start: toNumber time
        , end: toNumber time + unwrap duration
        , value
        }
