module Perusal.HTML (HTML, focus, sections) where

import Control.Monad.Eff (Eff)
import Data.Array (mapWithIndex)
import Data.Traversable (sequence_)
import DOM (DOM)
import Prelude (Unit, ($), (<<<), (==))

-- | A made-up type to represent our elements.
foreign import data HTML :: *

-- | A list of all the "section" elements on the page.
foreign import sections :: forall e
                         . Eff (dom :: DOM | e) (Array HTML)

-- | Change the display setting on a given HTML element.
foreign import display :: forall e
                        . String
                       -> HTML
                       -> Eff (dom :: DOM | e) Unit

-- | Show one particular section, and hide all the others.
focus :: forall e. Int -> Array HTML -> Eff (dom :: DOM | e) Unit
focus choice = traverseWithIndex mapper
  where traverseWithIndex f = sequence_ <<< mapWithIndex f
        mapper index = display $ if choice == index
                                   then "block"
                                   else "none"
