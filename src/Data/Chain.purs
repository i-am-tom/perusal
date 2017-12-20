module Data.Chain where

import Prelude

import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable, toUnfoldable) as L
import Data.List (reverse)
import Data.List.Types (List(Cons, Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)

newtype Chain a = Chain { before :: List a, after :: List a }

derive instance newtypeChain :: Newtype (Chain a) _
derive instance genericChain :: Generic (Chain a) _

instance eqChain :: Eq a => Eq (Chain a) where
  eq (Chain x) (Chain y)
     = x.before == y.before
    && x.after  == y.after

instance showChain :: Show a => Show (Chain a) where
  show (Chain { before, after })
    = show (reverse before) <> " | " <> show after

instance semigroupChain :: Semigroup (Chain a) where
  append (Chain { before, after }) that
    = Chain { before, after: after <> L.fromFoldable that }

instance functorChain :: Functor Chain where
  map f (Chain { before, after })
    = Chain
        { before: map f before
        , after:  map f after
        }

instance foldableChain :: Foldable Chain where
  foldr f acc (Chain { before, after })
    = foldr f acc (reverse before <> after)

  foldl f acc (Chain { before, after })
    = foldl f acc (reverse before <> after)

  foldMap f (Chain { before, after })
    = foldMap f (reverse before <> after)

instance applyChain :: Apply Chain where
  apply = ap

instance applicativeChain :: Applicative Chain where
  pure x = Chain { before: Nil, after: Cons x Nil }

instance bindChain :: Bind Chain where
  bind (Chain { before, after }) f
    = Chain
        { before: flatten (map f before)
        , after:  flatten (map f after)
        }
    where
      flatten = foldMap toUnfoldable

instance monadChain :: Monad Chain

instance extendChain :: Extend Chain where
  extend f xs
    = Chain
        { before: unfoldr (go <<<  left) xs
        , after:  unfoldr (go <<< right) xs
        }
    where go = map \(x /\ _) -> f x /\ x

fromFoldable :: forall t. Foldable t => t ~> Chain
fromFoldable xs = Chain { before: Nil, after: L.fromFoldable xs }

toUnfoldable :: forall f. Unfoldable f => Chain ~> f
toUnfoldable (Chain { before, after }) =
  L.toUnfoldable (reverse before <> after)

left :: forall a. Chain a -> Maybe (Chain a /\ a)
left (Chain { before: Nil }) = Nothing
left (Chain { before: Cons x before, after: xs }) =
  Just (Chain { before, after: Cons x xs } /\ x)

right :: forall a. Chain a -> Maybe (Chain a /\ a)
right (Chain { after: Nil }) = Nothing
right (Chain { before: xs, after: Cons x after })
  = Just (Chain { before: Cons x xs, after } /\ x)

