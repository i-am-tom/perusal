module Main (prepare) where

import Prelude
import Signal

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Argonaut.Core (Json)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Data.Config (read)
import DOM (DOM)
import Signal.DOM (keyPressed)

-- | Repeat a value whenever a boolean signal shows true.
replace :: forall a. a -> Signal Boolean -> Signal a
replace x signal = filter id false signal `sampleOn` constant x

-- | Convert arrow presses into a navigation signal.
navigation :: forall e. Int -> Eff (dom :: DOM | e) (Signal Int)
navigation boundary = do
    left  <- keyPressed 37 <#> replace (-1)
    right <- keyPressed 39 <#> replace   1

    pure (foldp add' 0 $ merge left right)
  where add' a b = clamp 0 (boundary - 1) $ a + b

prepare :: forall e
         . Json
        -> Eff ( console :: CONSOLE
               , dom :: DOM | e
               ) Unit
prepare json = do
    cursor <- navigation $ length config
    runSignal $ map logShow $ cursor ~> elem' config
  where
    config :: Array String
    config = read json

    elem' :: Array String -> Int -> String
    elem' xs n = case xs !! n of
      Just x  -> x
      Nothing -> "ERROR!!!"
