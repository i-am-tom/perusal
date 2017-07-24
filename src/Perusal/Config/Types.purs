module Perusal.Config.Types where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.ParentNode
  ( class IsParentNode
  , querySelector
  , querySelectorAll
  )
import DOM.Node.NodeList (toArray)
import DOM.Node.Types (Element)
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Perusal.Config.Parser.Types
  ( KeyframeSpec(KeyframeSpec)
  , SceneSpec(SceneSpec)
  , StyleSpec(StyleSpec)
  , ConfigSpec
  )
import Perusal.Easings (linear)


newtype Milliseconds = Milliseconds Int
derive instance newtypeMilliseconds :: Newtype Milliseconds _

newtype Style = Style
  { opacity    :: Number -> Number
  , rotate     :: Number -> Number
  , scale      :: Number -> Number
  , translateX :: Number -> Number
  , translateY :: Number -> Number
  }

derive instance newtypeStyle :: Newtype Style _

defaultStyle :: Style
defaultStyle = Style
  { opacity:    const 1.0
  , rotate:     const 0.0
  , scale:      const 1.0
  , translateX: const 0.0
  , translateY: const 0.0
  }

fromStyleSpec :: StyleSpec -> Style
fromStyleSpec (StyleSpec ss) = Style
    { opacity:    toEasing ss.opacity
    , rotate:     toEasing ss.rotate
    , scale:      toEasing ss.scale
    , translateX: toEasing ss.translateX
    , translateY: toEasing ss.translateY
    }
  where toEasing = uncurry linear


newtype Keyframe = Keyframe
  { duration :: Milliseconds
  , styles   :: Array (Tuple (Array Element) Style)
  }

derive instance newtypeKeyframe :: Newtype Keyframe _

fromKeyframeSpec :: forall document eff m
                  . IsParentNode document
                 => MonadError String m
                 => MonadEff (dom :: DOM | eff) m
                 => document
                 -> KeyframeSpec
                 -> m Keyframe
fromKeyframeSpec document (KeyframeSpec ks) = do
    let entries  = toUnfoldable ks.styles
    let duration = wrap ks.duration

    styles <- traverse prepare entries
    pure $ Keyframe { duration, styles }

  where prepare :: Tuple String StyleSpec
                -> m (Tuple (Array Element) Style)
        prepare (Tuple query style) = do
          list  <- liftEff $ querySelectorAll (wrap query) document
          nodes <- liftEff $ toArray list

          case traverse fromNode nodes of
            Just elems -> pure $ Tuple elems (fromStyleSpec style)
            Nothing    -> throwError "Keyframes contain non-elements!"


newtype Scene = Scene
  { container :: Element
  , frames    :: Array Keyframe
  }

derive instance newtypeScene :: Newtype Scene _

fromSceneSpec :: forall document eff m
               . IsParentNode document
              => MonadError String m
              => MonadEff (dom :: DOM | eff) m
              => document
              -> SceneSpec
              -> m Scene
fromSceneSpec document (SceneSpec ss) = do
  element <- liftEff $ querySelector (wrap ss.container) document
  frames  <- traverse (fromKeyframeSpec document) ss.frames

  case element of
    Just container -> pure $ Scene { container, frames }
    Nothing        -> throwError "Couldn't find container!"


type Config = Array Scene

fromConfigSpec :: forall document eff m
                . IsParentNode document
               => MonadError String m
               => MonadEff (dom :: DOM | eff) m
               => document
               -> ConfigSpec
               -> m Config
fromConfigSpec = traverse <<< fromSceneSpec
