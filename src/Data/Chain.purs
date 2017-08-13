module Data.Chain
  ( Chain
  , fromFoldable
  , toUnfoldable
  , left
  , right
  ) where

import Prelude

import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Function (on)
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
-- | as a list zipper, but without the "focus" element. What I mean is
-- | that it represents some point in "progress" from one end of a
-- | list to another.
-- |
-- | ```purescript
-- | Chain (Tuple [] [1, 2, 3]) -- then step right...
-- | Chain (Tuple [1] [2, 3]) -- then right again...
-- | Chain (Tuple [2, 1] [3])
-- | ```
-- |
-- | _For this particular bit of wisdom, I owe a debt to Phil Freeman
-- | and Nicholas Scheel (@paf31 and @monoidmusician, respectively).
-- | I was *totally* overthinking this._
newtype Chain a = Chain (Tuple (List a) (List a))


derive instance newtypeChain :: Newtype (Chain a) _
derive instance genericChain :: Generic (Chain a) _
derive instance eqChain :: Eq a => Eq (Chain a)


instance ordChain :: Ord a => Ord (Chain a) where
  compare :: Chain a -> Chain a -> Ordering
  compare = compare `on` (toUnfoldable :: Chain ~> Array)


instance semigroupChain :: Semigroup (Chain a) where
  append :: Chain a -> Chain a -> Chain a
  append (Chain (Tuple ps ns)) ys =
    Chain (Tuple ps (ns <> L.fromFoldable ys))


instance monoidChain :: Monoid (Chain a) where
  mempty :: Chain a
  mempty = Chain (Tuple Nil Nil)


instance functorChain :: Functor Chain where
  map :: forall a b. (a -> b) -> Chain a -> Chain b
  map f (Chain (Tuple xs ys)) =
    Chain (Tuple (map f xs) (map f ys))


instance foldableChain :: Foldable Chain where
  foldMap :: forall a m. Monoid m => (a -> m) -> Chain a -> m
  foldMap f (Chain (Tuple xs ys)) =
    foldMap f (reverse xs <> ys)

  -- Turns out that eager evaluation causes us an issue here: if we
  -- try to use the defaults, which rely on `foldMap`, the complaint
  -- is that `foldMap` hasn't been defined. So, we need to hide it
  -- inside a closure. @garyb gives a much better explanation:
  -- https://stackoverflow.com/a/38490637/4022180
  foldr f = foldrDefault f
  foldl f = foldlDefault f


instance applyChain :: Apply Chain where
  apply :: forall a b. Chain (a -> b) -> Chain a -> Chain b
  apply = ap


instance applicativeChain :: Applicative Chain where
  pure :: forall a. a -> Chain a
  pure x = Chain (Tuple Nil (Cons x Nil))


instance bindChain :: Bind Chain where
  bind :: forall a b. Chain a -> (a -> Chain b) -> Chain b
  bind (Chain (Tuple xs ys)) f =
    Chain $ Tuple (flatten ls) (flatten $ map f ys)
    where
      ls = reverse $ map f $ reverse xs
      flatten = foldMap toUnfoldable


instance monadChain :: Monad Chain

instance extendChain :: Extend Chain where
  extend :: forall a b. (Chain a -> b) -> Chain a -> Chain b
  extend f xs = Chain $ Tuple (unfoldr (go <<< left)  xs)
                              (unfoldr (go <<< right) xs)
    where go = map \(Tuple x _) -> Tuple (f x) x



-- | Transform a `Foldable` type into a `Chain`, where ethe pointer is
-- | right at the beginning (i.e. the `before` list is empty).
fromFoldable :: forall f. Foldable f => f ~> Chain
fromFoldable xs = Chain (Tuple Nil (L.fromFoldable xs))


-- | Transform a `Chain` into a desired `Unfoldable` type by remaking
-- | the list underneath.
toUnfoldable :: forall t. Unfoldable t => Chain ~> t
toUnfoldable (Chain (Tuple xs ys)) =
  L.toUnfoldable (reverse xs <> ys)


-- | Move to the `left`. If we can't, it's a `Nothing`. If we can, we
-- | get the `Chain` in the new position, and the value we passed.
left :: forall a. Chain a -> Maybe (Tuple (Chain a) a)
left (Chain (Tuple Nil _)) = Nothing
left (Chain (Tuple (Cons x xs) ys)) =
  Just (Tuple (Chain (Tuple xs (Cons x ys))) x)


-- | Like `left`, but, y'know, backwards.
right :: forall a. Chain a -> Maybe (Tuple (Chain a) a)
right (Chain (Tuple _ Nil)) = Nothing
right (Chain (Tuple xs (Cons y ys))) =
  Just (Tuple (Chain (Tuple (Cons y xs) ys)) y)
