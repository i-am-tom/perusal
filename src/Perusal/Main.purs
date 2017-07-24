module Perusal.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadError)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (document, alert)
import DOM.Node.Document (getElementsByTagName)
import Data.Maybe (Maybe(..))
import FRP (FRP)
import FRP.Behavior (animate)
import Perusal.HTML (toSlides, render)
import Perusal.Navigation (movement)
import Prelude ((=<<), (<$>), bind, Unit)

type App a = forall eff m
           . MonadEff (dom :: DOM | eff) m
          => MonadError String m
          => m a

-- | Get the slides, attach the controls, run the thingy!
main :: forall eff
      . Eff (alert :: ALERT, dom :: DOM, frp :: FRP | eff) Unit
main = do
  window'   <- window
  document' <- htmlDocumentToDocument <$> document window'
  tape      <- toSlides =<< getElementsByTagName "section" document'

  case tape of
    Just slides -> animate (movement slides) render
    Nothing -> alert "Where are your slides?" window'
