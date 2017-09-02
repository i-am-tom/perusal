module Data.Chain
  ( Chain
  , fromFoldable
  , toUnfoldable
  , left
  , left'
  , right
  , right'
  ) where

import Prelude

import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable, toUnfoldable) as L
import Data.List (reverse)
import Data.List.Types (List(Cons, Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr)


-- | A `Chain` is a name I made up for a type that is almost the same
-- as a list zipper, but without the "focus" element. What I mean is
-- that it represents some point in "progress" from one end of a list
-- list to another.
--
-- ```purescript
-- Chain { before:     [], after: [1, 2, 3] } -- then step right...
-- Chain { before:    [1], after:    [2, 3] } -- then right again...
-- Chain { before: [2, 1]  after:       [3] }
-- ```
--
-- _For this particular bit of wisdom, I owe a debt to Phil Freeman
-- and Nicholas Scheel (@paf31 and @monoidmusician, respectively). I
-- was *totally* overthinking this._
newtype Chain a = Chain { before :: List a, after :: List a }


derive instance newtypeChain :: Newtype (Chain a) _
derive instance genericChain :: Generic (Chain a) _

---

-- | Two chains are equivalent if they hold the same list and are at
-- the same point in their progression.
instance eqChain :: Eq a => Eq (Chain a) where
  eq (Chain x) (Chain y)
    =  x.before == y.before
    && x.after  == y.after

-- | If you ever want to debug a `Chain`, this function will print the
-- list and mark where we currently are within it.
instance showChain :: Show a => Show (Chain a) where
  show (Chain { before, after })
    = show (reverse before) <> " | " <> show after

-- | Add the whole of the RHS to the `after` list on the LHS. This,
-- unfortunately, means that we can't have a `Monoid` instance.
instance semigroupChain :: Semigroup (Chain a) where
  append :: Chain a -> Chain a -> Chain a
  append (Chain { before, after }) that
    = Chain { before, after: after <> L.fromFoldable that }


-- | Map a function over the items in this `Chain`.
instance functorChain :: Functor Chain where
  map :: forall a b. (a -> b) -> Chain a -> Chain b
  map f (Chain { before, after })
    = Chain
        { before: map f before
        , after:  map f after
        }


-- | Deconstruct the type with given reducers.
instance foldableChain :: Foldable Chain where
  -- | Turns out that eager evaluation causes us an issue here: if we
  -- try to use the defaults, which rely on `foldMap`, the complaint
  -- is that `foldMap` hasn't been defined. So, we need to hide it
  -- inside a closure. @garyb gives a much better explanation:
  -- https://stackoverflow.com/a/38490637/4022180
  foldr f = foldrDefault f
  foldl f = foldlDefault f

  foldMap :: forall a m. Monoid m => (a -> m) -> Chain a -> m
  foldMap f (Chain { before, after })
    = foldMap f (reverse before <> after)


-- | Merge two `Chain` values with a list-like `<*>`, but moving away
-- (in both directions) from the focus of the list.
instance applyChain :: Apply Chain where
  apply :: forall a b. Chain (a -> b) -> Chain a -> Chain b
  apply = ap


-- | Lift a single value into a `Chain`.
instance applicativeChain :: Applicative Chain where
  pure :: forall a. a -> Chain a
  pure x = Chain { before: Nil, after: Cons x Nil }


-- | Almost identical to a list bind, but the position is maintained,
-- and all operations happen as if "moving away" from the focus.
instance bindChain :: Bind Chain where
  bind :: forall a b. Chain a -> (a -> Chain b) -> Chain b
  bind (Chain { before, after }) f
    = Chain
        { before: flatten before' 
        , after:  flatten (map f after)
        }
    where
      -- Known bug: signatures in `where` clause under instance
      -- signatures. Where my ``RankNTypes` at, huh?

      -- ls :: Array b
      before' = reverse (map f (reverse before))

      -- flatten :: Array b
      flatten = foldMap toUnfoldable


instance monadChain :: Monad Chain


-- | To extend, we call the given function on the `Chain` in all its
-- possible configurations, and make a `Chain` of the results.
instance extendChain :: Extend Chain where
  extend :: forall a b. (Chain a -> b) -> Chain a -> Chain b
  extend f xs
    = Chain
        { before: unfoldr (go <<<  left') xs
        , after:  unfoldr (go <<< right') xs
        }
    where
      go = map \(Tuple x _) -> Tuple (f x) x


-- | Transform a `Foldable` type into a `Chain`, where the pointer is
-- right at the beginning (i.e. the `before` list is empty).
fromFoldable :: forall f. Foldable f => f ~> Chain
fromFoldable xs = Chain { before: Nil, after: L.fromFoldable xs }


-- | Transform a `Chain` into a desired `Unfoldable` type by remaking
-- the list underneath.
toUnfoldable :: forall t. Unfoldable t => Chain ~> t
toUnfoldable (Chain { before, after }) =
  L.toUnfoldable (reverse before <> after)


-- | Move to the `left`, and apply some transformation to the element
-- that we pass in the process. If it works, of course.
left :: forall a b. (a -> b) -> Chain a -> Maybe (Tuple (Chain a) b)
left _ (Chain { before: Nil }) = Nothing
left f (Chain { before: Cons x before, after: xs }) =
  Just (Tuple (Chain { before, after: Cons x xs }) (f x))

-- | Move to the `left`, and return the passed element. `left id`, in
-- short.
left' :: forall a. Chain a -> Maybe (Tuple (Chain a) a)
left' = left id

-- | Like `left`, but, y'know, backwards.
right :: forall a b. (a -> b) -> Chain a -> Maybe (Tuple (Chain a) b)
right _ (Chain { after: Nil }) = Nothing
right f (Chain { before: xs, after: Cons x after })
  = Just (Tuple (Chain { before: Cons x xs, after }) (f x))

-- | Like `left'`, but, y'know, backwards.
right' :: forall a. Chain a -> Maybe (Tuple (Chain a) a)
right' = right id
