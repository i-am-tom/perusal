module Data.Tape where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array (cons, uncons)
import Data.Foldable
  ( class Foldable
  , foldl
  , foldMap
  , foldr
  )
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Profunctor.Strong ((&&&))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Unfoldable (unfoldr)

-- | A non-empty array with a read head.
data Tape a = Tape (Array a) a (Array a)

-- | Produce a tape (focused on the first element) from a array.
fromArray :: forall a. Array a -> Maybe (Tape a)
fromArray = map (\{ head, tail } -> Tape [] head tail) <<< uncons

-- | Convert a non-empty array to a tape.
fromNonEmpty :: NonEmpty Array ~> Tape
fromNonEmpty (x :| xs) = Tape [] x xs

-- | Attempt to move the head to the left.
left :: forall a. Tape a -> Maybe (Tape a)
left (Tape ls x rs) = map shift (uncons ls)

  where shift :: { head :: a, tail :: Array a } -> Tape a
        shift { head, tail } = Tape tail head (cons x rs)

-- | Attempt to move the head to the right.
right :: forall a. Tape a -> Maybe (Tape a)
right = map reverse <<< left <<< reverse

-- | Flip the entire tape around the read head.
reverse :: forall a. Tape a -> Tape a
reverse (Tape ls x rs) = Tape rs x ls

-- | Show the tape with the read head position.
instance showTape :: Show a => Show (Tape a) where
  -- | Represent a tape with a string.
  show :: forall a. Show a => Tape a -> String
  show (Tape ls x rs) = left' <> middle <> right'

    where left'  = foldr (\l acc -> acc <> show l <> " ") "" ls
          middle = "[" <> show x <> "]"
          right' = foldl (\acc r -> acc <> " " <> show r) "" rs

-- | Equality between two tapes.
derive instance eqTape :: Eq a => Eq (Tape a)

-- | Ordering between two tapes.
derive instance ordTape :: Ord a => Ord (Tape a)

-- | Transform every item on the tape.
derive instance functorTape :: Functor Tape

-- | Reduce a Tape to a single value.
instance foldableTape :: Foldable Tape where
  -- | Fold a array monoidally.
  foldMap f (Tape ls x rs) = foldr ((<>) <<< f) mempty ls
                          <> f x
                          <> foldMap f rs

  -- | Fold a tape from the right across.
  foldr :: forall a b. (a -> b -> b) -> b -> Tape a -> b
  foldr f acc (Tape ls x rs) = foldl (flip f) (f x (foldr f acc rs)) ls

  -- | Fold a tape from the left across.
  foldl :: forall a b. (b -> a -> b) -> b -> Tape a -> b
  foldl f acc (Tape ls x rs) = foldr (flip f) (f (foldl f acc ls) x) rs

-- | "Flip" a context.
instance traversableTape :: Traversable Tape where
  -- | Apply a contextual operation over a tape.
  traverse :: forall a b f. Applicative f => (a -> f b) -> Tape a -> f (Tape b)
  traverse f (Tape ls x rs) = Tape <$> traverse f ls <*> f x <*> traverse f rs

  -- | Pull an inner context to the outside of a Tape.
  sequence :: forall a f. Applicative f => Tape (f a) -> f (Tape a)
  sequence (Tape ls x rs) = Tape <$> sequence ls <*> x <*> sequence rs

-- | Return the tape after the function has been applied
-- | with the read head in all possible positions.
instance extendTape :: Extend Tape where
  -- | https://github.com/purescript/purescript/issues/2941
  extend :: forall a b. (Tape a -> b) -> Tape a -> Tape b
  extend f xs = Tape (go left xs) (f xs) (go right xs)

    where -- go :: (Tape a -> Maybe (Tape a)) -> Tape a -> Array b
          go t = unfoldr $ map (f &&& id) <<< t

-- | Get the item under the read head.
instance comonadTape :: Comonad Tape where
  extract :: forall a. Tape a -> a
  extract (Tape _ x _) = x
