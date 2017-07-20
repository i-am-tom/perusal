module Perusal.HTML (render, toSlides) where

import Prelude ((<<<), (<>), (*>), map, Unit)

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Data.Tape (fromArray, Tape(Tape))
import Data.Traversable (traverse)

import DOM (DOM)
import DOM.Node.Element (setAttribute)
import DOM.Node.HTMLCollection (toArray)
import DOM.Node.Types (HTMLCollection, Element)

-- | Show the focus element, hide everything else! At the moment, this
-- | is a pretty naïve approach: we override the style to "block" or
-- | "none", depending on its state.
render :: forall eff
        . Tape Element
        -> Eff ( dom :: DOM | eff) Unit
render (Tape ls x rs) = traverse hide (ls <> rs) *> show x

  where hide :: Element -> Eff (dom :: DOM | eff) Unit
        hide = setAttribute "style" "display:none"

        show :: Element -> Eff (dom :: DOM | eff) Unit
        show = setAttribute "style" "display:block"

-- | Convert an HTML selection to a slide deck! Not very exciting; we
-- | convert an HTMLCollection to Array Element, then to a Tape.
toSlides :: forall eff
          . HTMLCollection
         -> Eff (dom :: DOM | eff) (Maybe (Tape Element))
toSlides = map fromArray <<< toArray
