module Main where

import Prelude (Unit, ($), bind, flip)
import Signal ((~>), runSignal)

import Control.Monad.Eff (Eff)
import Data.Array (length)
import Perusal.HTML (focus, sections)
import Perusal.Navigation (navigation)
import DOM (DOM)

main :: forall e. Eff (dom :: DOM | e) Unit
main = do
  slides <- sections
  cursor <- navigation $ length slides

  runSignal $ cursor ~> flip focus slides
