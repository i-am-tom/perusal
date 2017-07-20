module Main where

import Prelude ((=<<), (<$>), bind, Unit)

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (document, alert)
import DOM.Node.Document (getElementsByTagName)
import FRP (FRP)
import FRP.Behavior (animate)

import Perusal.HTML (toSlides, render)
import Perusal.Navigation (movement)

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
