module Perusal.HTML (render) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import DOM (DOM)
import DOM.Node.Element (setAttribute)
import DOM.Node.Types (Element)
import Data.Foldable (for_)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(Tuple), fst)
import Math as M
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

    <> "rotate(" <> show (round $ rotate * 2.0 * M.pi) <> "rad) "
    <> "scale(" <> show scale <> ")"
  where

    round :: Number -> Number
    round = (_ / 1000.0) <<< M.round <<< (1000.0 * _)

-- | Now, we get to the interesting bit. Thanks to the shape of our
-- | `RenderFrame` type, we're kinda spoilt at this point. We hide the
-- | last container, show the new one, and then apply all the styles.
-- | Nothing scary to do, no frightening tricks.
render :: forall eff m
        . MonadEff (dom :: DOM | eff) m
       => { last :: Maybe RenderFrame, now :: RenderFrame }
       -> m Unit
render { last, now: Tuple container keyframes } =
  liftEff $ maybe (pure unit) (hide <<< fst) last
         *> setAttribute "style" "" container
         *> for_ keyframes \(Tuple elements style) ->
              for_ elements $ setAttribute "style" (toCSS style)
  where

    hide :: forall eff'. Element -> Eff (dom :: DOM | eff') Unit
    hide = setAttribute "style" "display: none"
