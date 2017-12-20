module Perusal.Config.Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.ParentNode (class IsParentNode, querySelector, querySelectorAll)
import DOM.Node.NodeList (toArray)
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.Types (Element)
import Data.Chain (Chain, fromFoldable)
import Data.Foldable (fold)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (values)
import Data.Traversable (sequence, traverse)
import Easing (polynomial)
import Perusal.Config.Parser.Types as Parsed

newtype Style
  = Style
      { opacity    :: Number
      , rotate     :: Number
      , scale      :: Number
      , translateX :: Number
      , translateY :: Number
      }

derive instance newtypeStyle :: Newtype Style _

fromStyleSpec :: Parsed.Style -> Number -> Style
fromStyleSpec (Parsed.Style ss) progress
  = Style
      { opacity:    process ss.opacity    1.0
      , rotate:     process ss.rotate     0.0
      , scale:      process ss.scale      1.0
      , translateX: process ss.translateX 0.0
      , translateY: process ss.translateY 0.0
      }
  where
    ease (Parsed.Transition { to, from })
      = polynomial 2.0 to from progress

    process (NullOrUndefined value) fallback
      = maybe fallback ease value

newtype Styler
  = Styler
    { elements     :: Array Element
    , fromProgress :: Number -> Style
    }

reverse :: Styler -> Styler
reverse (Styler s@{ fromProgress })
  = Styler (s { fromProgress = map fromProgress (1.0 - _) })

derive instance newtypeStyler :: Newtype Styler _

newtype Keyframe
  = Keyframe
    { duration :: Number
    , stylers  :: Array Styler
    }

derive instance newtypeKeyframe :: Newtype Keyframe _

fromKeyframeSpec
  :: forall document eff
   . IsParentNode document
  => document
  -> Parsed.Keyframe
  -> ExceptT String (Eff (dom :: DOM | eff)) Keyframe
fromKeyframeSpec document (Parsed.Keyframe keyframe)
  = construct <$> ( if keyframe.duration < 0.0
                      then throwError "Negative direction!"
                      else pure keyframe.duration )
              <*> sequence (values (mapWithIndex prepare keyframe.styles))
  where
    construct duration stylers
      = Keyframe { duration, stylers }

    prepare query styleSpec
      = do
          let style = fromStyleSpec styleSpec
          nodes <- lift (querySelectorAll (QuerySelector query) document >>= toArray)

          case traverse fromNode nodes of
            Nothing -> throwError "Keyframes contain non-elements!"
            Just xs -> pure (Styler { elements: xs, fromProgress: style })

freeze :: Number -> Keyframe -> Array { elements :: Array Element, style :: Style }
freeze elapsed (Keyframe { duration, stylers }) = map go stylers
  where go (Styler { elements, fromProgress })
          = { elements, style: fromProgress (elapsed / duration) }

newtype Scene
  = Scene
    { container :: Element
    , keyframes :: Array Keyframe
    }

derive instance newtypeScene :: Newtype Scene _

fromSceneSpec
  :: forall document eff
   . IsParentNode document
  => document
  -> Parsed.Scene
  -> ExceptT String (Eff (dom :: DOM | eff)) Scene
fromSceneSpec document (Parsed.Scene scene) = do
    container <- container'
    keyframes <- keyframes'

    pure $ Scene { container, keyframes }

  where
    container' :: ExceptT String (Eff (dom :: DOM | eff)) Element
    container' = (lift $ querySelector (QuerySelector scene.container) document)
      >>= maybe (throwError ("Can't find " <> scene.container <> "!")) pure

    keyframes' :: ExceptT String (Eff (dom :: DOM | eff)) (Array Keyframe)
    keyframes' = traverse (fromKeyframeSpec document) (fold (unwrap scene.keyframes))

flatten :: Scene -> Array { container :: Element, keyframe :: Keyframe }
flatten (Scene { container, keyframes })
  = map { container, keyframe: _ } keyframes

toChain :: Config -> Chain { container :: Element, keyframe :: Keyframe }
toChain (Config scenes)
  = fromFoldable $ scenes >>= flatten

newtype Config = Config (Array Scene)

derive instance newtypeConfig :: Newtype Config _

fromConfigSpec
  :: forall document eff
   . IsParentNode document
  => document
  -> Parsed.Config
  -> ExceptT String (Eff (dom :: DOM | eff)) Config
fromConfigSpec doc (Parsed.Config config)
  = Config <$> traverse (fromSceneSpec doc) config

