module Test.Main where

import Prelude (($), (<>), (<$>), (>>=), Unit, discard, id, join, pure)

import Control.Comonad (extract)
import Control.Extend (extend)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (reverse, uncons) as A
import Data.Foldable (foldl, foldMap, foldr)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tape
import Data.Traversable (sequence, traverse)

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

  log "foldl"
  quickCheck $ \ls (x :: Array Int) rs ->
    join (A.reverse ls) <> x <> join rs
      === foldl (<>) [] (Tape ls x rs)

  log "foldr"
  quickCheck $ \ls (x :: Array Int) rs ->
    join (A.reverse ls) <> x <> join rs
      === foldr (<>) [] (Tape ls x rs)

  log "foldMap"
  quickCheck $ \ls (x :: Array Int) rs ->
    join (A.reverse ls) <> x <> join rs
      === foldMap id (Tape ls x rs)

  log "traverse"
  quickCheck $ \ls (x :: Int) rs ->
    traverse pure (Tape ls x rs)
      === [Tape ls x rs]

  log "sequence"
  quickCheck $ \ls (x :: Int) rs ->
    let
      f x = [x]
    in
      sequence (f <$> Tape ls x rs)
        === traverse f (Tape ls x rs)

  log "extend"
  quickCheck $ \ls (x :: Int) rs ->
    extend extract (Tape ls x rs) === Tape ls x rs

  log "extract"
  quickCheck $ \ls (x :: Int) rs ->
    extract (Tape ls x rs) === x
