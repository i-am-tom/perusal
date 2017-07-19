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

-- | Show the focus element, hide everything else!
render :: forall eff
        . Tape Element
        -> Eff ( dom :: DOM | eff) Unit
render (Tape ls x rs) = traverse hide (ls <> rs) *> show x

  where hide :: Element -> Eff (dom :: DOM | eff) Unit
        hide = setAttribute "style" "display:none"

        show :: Element -> Eff (dom :: DOM | eff) Unit
        show = setAttribute "style" "display:block"

-- | Convert an HTML selection to a slide deck!
toSlides :: forall eff
          . HTMLCollection
         -> Eff (dom :: DOM | eff) (Maybe (Tape Element))
toSlides = map fromArray <<< toArray
