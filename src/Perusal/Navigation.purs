module Perusal.Navigation (navigation) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude ((<#>), ($), (+), (-), bind, clamp, id, negate, pure)
import Signal (Signal, constant, filter, foldp, merge, sampleOn)
import Signal.DOM (keyPressed)

-- | Return a given value whenever a given signal be true.
replace :: forall a. a -> Signal Boolean -> Signal a
replace x signal = whenTrue signal `sampleOn` constant x
  where whenTrue = filter id false

-- | Move a cursor within a range from 0 to a boundary. The cursor's
-- | movements are controlled by left and right arrow key presses.
navigation :: forall e. Int -> Eff (dom :: DOM | e) (Signal Int)
navigation boundary = do
    left  <- keyPressed 37 <#> replace (-1)
    right <- keyPressed 39 <#> replace   1

    pure $ foldp add' 0 $ left `merge` right
  where
    add' :: Int -> Int -> Int
    add' a b = clamp 0 (boundary - 1) $ a + b
