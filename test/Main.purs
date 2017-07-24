module Test.Main where

import Prelude

import Control.Comonad (extract)
import Control.Extend (extend)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (reverse, uncons) as A
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tape
  ( Tape(..)
  , fromArray
  , fromNonEmpty
  , left, right
  , reverse
  , toArray
  )
import Data.Traversable (traverse)
import Test.QuickCheck (quickCheck, (===))

main :: forall eff. Eff ( console :: CONSOLE
                        , random :: RANDOM
                        , exception :: EXCEPTION
                        | eff
                        ) Unit
main = do

  log "fromArray"
  quickCheck $ \(arr :: Array Int) -> do
    case A.uncons arr of
      Just { head, tail } -> fromArray arr === (Just $ Tape [] head tail)
      Nothing             -> arr === []

  log "fromNonEmpty"
  quickCheck $ \arr@((x :: Int) :| xs) ->
    fromNonEmpty arr === Tape [] x xs

  log "left"
  quickCheck $ \ls (x :: Int) rs -> do
    let t = Tape ls x rs

    case ls of
      [] -> left t === Nothing
      _  -> Just t === (left t >>= right)


  log "right"
  quickCheck $ \ls (x :: Int) rs -> do
    let t = Tape ls x rs

    case rs of
      [] -> right t === Nothing
      _  -> Just t === (right t >>= left)

  log "reverse"
  quickCheck $ \ls (x :: Int) rs ->
    (reverse $ Tape ls x rs) === Tape rs x ls

  log "foldMap"
  quickCheck $ \ls (x :: Array Int) rs ->
    join (A.reverse ls) <> x <> join rs
      === foldMap id (Tape ls x rs)

  log "traverse"
  quickCheck $ \ls (x :: Int) rs ->
    traverse pure (Tape ls x rs)
      === [Tape ls x rs]

  log "bind"
  quickCheck $ \ls (x :: Int) rs ->
    (toArray $ (Tape ls x rs) >>= \y -> Tape [y] y [y])
      === ((A.reverse ls <> [x] <> rs) >>= \y -> [y, y, y])

  log "extend"
  quickCheck $ \ls (x :: Int) rs ->
    extend extract (Tape ls x rs) === Tape ls x rs

  log "extract"
  quickCheck $ \ls (x :: Int) rs ->
    extract (Tape ls x rs) === x
