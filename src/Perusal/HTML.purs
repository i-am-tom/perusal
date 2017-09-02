module Perusal.HTML (render) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import DOM (DOM)
import DOM.Node.Element (setAttribute)
import DOM.Node.Types (Element)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe, maybe)
import Math as M
import Perusal.Config.Types (FrozenStyle(..), Style(Style))

-- | When all is said and all is done, we still need to turn a `Style`
-- into CSS to attach it to the elements. It doesn't feel right to
-- use `Show` here, as we'd probably want a clearer representation,
-- so we'll write this little scamp. Take a `Style`, make a CSS.
toCSS :: Style -> String
toCSS (Style { opacity, rotate, scale, translateX, translateY })
  =  "opacity: " <> show opacity <> "; "
  <> "transform: "
    <> "translate2d("
      <> show translateX <> ", "
      <> show translateY <> ") "

    <> "rotate(" <> show (round $ rotate * 2.0 * M.pi) <> "rad) "
    <> "scale(" <> show scale <> ")"
  where
    -- TODO: benchmark. I'm not sure this is a good idea...
    round :: Number -> Number
    round = (_ / 1000.0) <<< M.round <<< (1000.0 * _)

-- | When we want to render a frame, we need the container, and the
-- styles to apply.
type RenderFrame
  = { container :: Element
    , styles :: Array FrozenStyle
    }

-- | Render the current state of the application as a change from the
-- previous state. Hide the old container, show the new one. Set the
-- appropriate styles. A nice TODO here would be to check whether the
-- containers match and not bother with the visibility flipping.
render
  :: forall eff m.
     MonadEff (dom :: DOM | eff) m
  => { last :: Maybe RenderFrame, now :: RenderFrame }
  -> m Unit
render { last, now: { container, styles } }
  =  maybe (pure unit) (hide <<< _.container) last
  *> show container
  *> traverse_ set styles
  where

    -- | Hide the given element with `display: none`.
    hide :: Element -> m Unit
    hide = liftEff
       <<< setAttribute "style" "display: none"

    -- | Show the given element by removing the `style`.
    show :: Element -> m Unit
    show = liftEff
       <<< setAttribute "style" ""

    -- | Apply a given style to a given element!
    styler :: Style -> Element -> m Unit
    styler style
      = liftEff
      <<< setAttribute "style" (toCSS style)

   -- | Set specific styling on an element.
    set :: FrozenStyle -> m Unit
    set (FrozenStyle { elements, style })
      = traverse_ (styler style) elements
