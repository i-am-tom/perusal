module Perusal.HTML (init, render) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Element (setAttribute)
import DOM.Node.Types (Element)
import Data.Foldable (foldr, for_)
import Data.Maybe (Maybe(..), maybe)
import Math as M
import Perusal.Config.Types as Types

init :: forall eff. Types.Config -> Eff (dom :: DOM | eff) Unit
init (Types.Config scenes) = do
  let
    rewind now prev = prev >>= \last -> render { last, now } $> Just now

    flattened :: Array RenderFrame
    flattened = scenes >>= \(Types.Scene { container, keyframes }) ->
      map ({ container, styles: _ } <<< Types.freeze 0.0) keyframes

  void $ foldr rewind (pure Nothing) flattened

toCSS :: Types.Style -> String
toCSS (Types.Style { opacity, rotate, scale, translateX, translateY })
  =  "opacity: " <> show opacity <> "; "
  <> "transform: "
    <> "translate2d("
      <> show translateX <> ", "
      <> show translateY <> ") "

    <> "rotate(" <> show (rotate * 2.0 * M.pi) <> "rad) "
    <> "scale(" <> show scale <> ")"

type RenderFrame
  = { container
        :: Element

    , styles
        :: Array
             { elements :: Array Element
             , style :: Types.Style
             }
    }

render
  :: forall eff
   . { last :: Maybe RenderFrame
     , now  ::       RenderFrame
     }
  -> Eff (dom :: DOM | eff) Unit
render { last, now: { container, styles } }
  =  maybe (pure unit) (hide <<< _.container) last
  *> show container
  *> for_ styles set
  where
    hide = setAttribute "style" "display: none"
    show = setAttribute "style" ""

    set { elements, style } = for_ elements (setAttribute "style" (toCSS style))
