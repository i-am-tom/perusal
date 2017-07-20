module Data.Tape where

import Prelude ( class Eq
               , class Functor
               , class Ord
               , class Show
               , type (~>)
               , (<<<), (<*>), (<$>), (<>), ($)
               , compare, flip, id, map, pure, show
               )

import Control.Applicative (class Applicative)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array (cons, uncons)
import Data.Array (reverse) as A
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Function (on)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Profunctor.Strong ((&&&))
import Data.String (joinWith)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Unfoldable (unfoldr)


-- | A non-empty array with a read head. In literature, this is
-- | sometimes called a zip list or zipper. We store the "previous"
-- | list backwards so we can uncons easily. For example, the list of
-- | 1 through 5 with the read head over 3 would be represented as
-- | Tape [2, 1] 3 [4, 5].
data Tape a = Tape (Array a) a (Array a)


-- | Produce a tape (focused on the first element) from an array.
-- | Note that we can't make a Tape out of an empty array, hence the
-- | Maybe wrapper. All tapes are created with the read head at the
-- | start of the array.
fromArray :: forall a. Array a -> Maybe (Tape a)
fromArray = map (\{ head, tail } -> Tape [] head tail) <<< uncons


-- | Convert a non-empty array to a tape. Because we have a type-level
-- | guarantee that NonEmpty is... well, not empty, we don't need any
-- | Maybe wrapper around the result.
fromNonEmpty :: NonEmpty Array ~> Tape
fromNonEmpty (x :| xs) = Tape [] x xs


-- | Attempt to move the head to the left. Note that this will fail if
-- | the pointer is at the very left of the tape, as there's nowhere
-- | to move.
left :: forall a. Tape a -> Maybe (Tape a)
left (Tape ls x rs) = map shift (uncons ls)

  where shift :: { head :: a, tail :: Array a } -> Tape a
        shift { head, tail } = Tape tail head (cons x rs)


-- | Attempt to move the head to the right. Similarly to `left`, this
-- | will fail for any tape in which the read head is as far to the
-- | right as it can be.
right :: forall a. Tape a -> Maybe (Tape a)
right = map reverse <<< left <<< reverse


-- | Flip the entire tape around the read head. Note that this is an
-- | O(1) operation thanks to the reversed first list.
reverse :: forall a. Tape a -> Tape a
reverse (Tape ls x rs) = Tape rs x ls


instance showTape :: Show a => Show (Tape a) where

  -- | Represent a tape with a string. Each element is shown, space-
  -- | separated, and a pair of square brackets wrap the element under
  -- | the read head.
  show :: forall a. Show a => Tape a -> String
  show (Tape ls x rs) = joinWith " " (ls' <> x' <> rs')
    where ls' = map show $ A.reverse ls
          x'  = [ "[" <> show x <> "]" ]
          rs' = map show rs

-- | Check whether two tapes be equivalent by comparing the elements,
-- | as well as the position of the read head. O(n)
derive instance eqTape :: Eq a => Eq (Tape a)


instance ordTape :: Ord a => Ord (Tape a) where

  -- | A notion of order can be established for tapes by converting
  -- | them to arrays and comparing. The read head, in my opinion,
  -- | doesn't add to a notion of "order" at this level.
  compare = compare `on` (foldMap pure :: Tape ~> Array)


-- | Transform every item on the tape. PureScript can do the heavy
-- | lifting on this one, and save us some boilerplate!
derive instance functorTape :: Functor Tape


instance foldableTape :: Foldable Tape where

  -- | Fold a tape up into a monoid accumulator. Note that this works
  -- | from left-to-right, rather than in the way expressed by the
  -- | under-the-hood implementation of the tape.
  foldMap f (Tape ls x rs) = foldr (\l acc -> acc <> f l) mempty ls
                          <> f x
                          <> foldMap f rs


  -- | Fold a tape from the right across to the left.
  foldr :: forall a b. (a -> b -> b) -> b -> Tape a -> b
  foldr f acc (Tape ls x rs) = foldl (flip f) (f x (foldr f acc rs)) ls


  -- | Fold a tape from the left across to the right.
  foldl :: forall a b. (b -> a -> b) -> b -> Tape a -> b
  foldl f acc (Tape ls x rs) = foldl f (f (foldr (flip f) acc ls) x) rs


instance traversableTape :: Traversable Tape where

  -- | Apply an Applicative-returning function over a tape, and then
  -- | return the tape wrapped in that context.
  traverse :: forall a b f. Applicative f => (a -> f b) -> Tape a -> f (Tape b)
  traverse f (Tape ls x rs) = Tape <$> traverse f ls <*> f x <*> traverse f rs

  -- | Pull an inner context to the outside of a Tape.
  sequence :: forall a f. Applicative f => Tape (f a) -> f (Tape a)
  sequence (Tape ls x rs) = Tape <$> sequence ls <*> x <*> sequence rs


instance extendTape :: Extend Tape where

  -- Apply an operation across the whole tape.
  -- | https://github.com/purescript/purescript/issues/2941
  extend :: forall a b. (Tape a -> b) -> Tape a -> Tape b
  extend f xs = Tape (go left xs) (f xs) (go right xs)

    where -- go :: (Tape a -> Maybe (Tape a)) -> Tape a -> Array b
          go t = unfoldr $ map (f &&& id) <<< t


instance comonadTape :: Comonad Tape where

  -- Extract the value under the read head.
  extract :: forall a. Tape a -> a
  extract (Tape _ x _) = x
