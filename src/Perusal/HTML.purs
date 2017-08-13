module Perusal.HTML (render) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import DOM (DOM)
import DOM.Node.Element (setAttribute)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Math (pi)
import Perusal.Config.Types (Style(Style), RenderFrame)

-- | When all is said and all is done, we still need to turn a `Style`
-- | into CSS to attach it to the elements. It doesn't feel right to
-- | use `Show` here, as we'd probably want a clearer representation,
-- | so we'll write this little scamp. Take a `Style`, make a CSS.
toCSS :: Style -> String
toCSS (Style { opacity, rotate, scale, translateX, translateY })
  =  "opacity: " <> show opacity <> "; "
  <> "transform: "
    <> "translate2d("
      <> show translateX <> ", "
      <> show translateY <> ") "

    <> "rotate(" <> show (rotate * 2.0 * pi) <> "rad) "
    <> "scale(" <> show scale <> ")"

-- | Ignore the `group` and `set` here if it's scary; chances are that
-- | you'll use `Array` for both! This function takes a list of
-- | `Element` / `Style` groups, and... well, it styles the elements.
render :: forall eff m
        . MonadEff (dom :: DOM | eff) m
       => { last :: Maybe RenderFrame, now :: RenderFrame }
       -> m Unit
render { last, now: Tuple c ks } =
  liftEff
     $ hide last
    *> setAttribute "style" "" c
    *> for_ ks \(Tuple elements style) ->
         for_ elements $ setAttribute "style" (toCSS style)
  where
    hide (Just (Tuple e _)) = setAttribute "style" "display: none" e
    hide Nothing = pure unit
