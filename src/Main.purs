module Main (load) where

import Prelude
import Signal

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array (length)
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

load :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
load = navigation 10 >>= runSignal <<< (_ ~> logShow)
