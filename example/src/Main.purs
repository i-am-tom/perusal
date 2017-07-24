module Example.Main (setup) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)

setup :: forall eff. Eff (dom :: DOM, alert :: ALERT | eff) Unit
setup = window >>= ("Where are all your slides?" `alert` _)
