module Perusal.Config.Types where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.ParentNode (class IsParentNode, querySelector, querySelectorAll)
import DOM.Node.NodeList (toArray)
import DOM.Node.Types (Element)
import Data.Chain (Chain, fromFoldable)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', traversed, (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.StrMap (values)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Easing (polynomial)
import Perusal.Config.Parser.Types
  ( ConfigSpec(..)
  , KeyframeSpec(..)
  , SceneSpec(..)
  , StyleSpec(..)
  )

---

newtype Style
  = Style
    { opacity    :: Number
    , rotate     :: Number
    , scale      :: Number
    , translateX :: Number
    , translateY :: Number
    }

derive instance newtypeStyle :: Newtype Style _

style_opacity :: Lens' Style Number
style_opacity = _Newtype <<< prop (SProxy :: SProxy "opacity")

style_rotate :: Lens' Style Number
style_rotate = _Newtype <<< prop (SProxy :: SProxy "rotate")

style_scale :: Lens' Style Number
style_scale = _Newtype <<< prop (SProxy :: SProxy "scale")

style_translateX :: Lens' Style Number
style_translateX = _Newtype <<< prop (SProxy :: SProxy "translateX")

style_translateY :: Lens' Style Number
style_translateY = _Newtype <<< prop (SProxy :: SProxy "translateY")

defaultStyle :: Style
defaultStyle
  = Style
    { opacity:    1.0
    , rotate:     0.0
    , scale:      1.0
    , translateX: 0.0
    , translateY: 0.0
    }

fromStyleSpec :: StyleSpec -> Number -> Style
fromStyleSpec (StyleSpec ss) progress
  = Style $ unwrap defaultStyle # \fallbacks ->
      { opacity: fromMaybe fallbacks.opacity
          $ parse <$> unwrap (ss.opacity)

      , rotate: fromMaybe fallbacks.rotate
          $ parse <$> unwrap (ss.rotate)

      , scale: fromMaybe fallbacks.scale
          $ parse <$> unwrap (ss.rotate)

      , translateX: fromMaybe fallbacks.translateX
          $ parse <$> unwrap (ss.translateX)

      , translateY: fromMaybe fallbacks.translateX
          $ parse <$> unwrap (ss.translateY)
      }
  where
    parse :: { to :: Number, from :: Number } -> Number
    parse { to, from } = polynomial 2.0 to from progress

---

newtype Styler
  = Styler
    { elements     :: Array Element
    , fromProgress :: Number -> Style
    }

derive instance newtypeStyler :: Newtype Styler _

styler_elements :: Lens' Styler (Array Element)
styler_elements = _Newtype <<< prop (SProxy :: SProxy "elements")

styler_fromProgress :: Lens' Styler (Number -> Style)
styler_fromProgress = _Newtype
  <<< prop (SProxy :: SProxy "fromProgress")

---

newtype Keyframe
  = Keyframe
    { duration :: Number
    , stylers  :: Array Styler
    }

derive instance newtypeKeyframe :: Newtype Keyframe _

keyframe_duration :: Lens' Keyframe Number
keyframe_duration = _Newtype <<< prop (SProxy :: SProxy "duration")

keyframe_styler :: Traversal' Keyframe (Number -> Style)
keyframe_styler = _Newtype <<< prop (SProxy :: SProxy "stylers")
                           <<< traversed
                           <<< styler_fromProgress

fromKeyframeSpec
  :: forall document eff m
   . IsParentNode document
  => MonadError String m
  => MonadEff (dom :: DOM | eff) m
  => document
  -> KeyframeSpec
  -> m Keyframe
fromKeyframeSpec document (KeyframeSpec ks)
  = map Keyframe $ { duration: _, stylers: _ }
      <$> ( if ks.duration < 0.0
              then throwError "Negative direction!"
              else pure ks.duration )
      <*> (sequence (values (mapWithIndex prepare ks.styles)))
  where

    prepare :: String -> StyleSpec -> m Styler
    prepare query style
      = do
          nodes <- liftEff
                       $ querySelectorAll (wrap query) document
                     >>= toArray

          case traverse fromNode nodes of
            Just elements -> pure
              $ Styler { elements
                       , fromProgress: fromStyleSpec style
                       }
            Nothing -> throwError "Keyframes contain non-elements!"

reverse :: Keyframe -> Keyframe
reverse = keyframe_styler %~ (_ <<< sub 1.0)

---

newtype FrozenStyle
  = FrozenStyle
    { elements :: Array Element
    , style    :: Style
    }

derive instance newtypeFrozenStyle :: Newtype FrozenStyle _

freeze :: Number -> Keyframe -> Array FrozenStyle
freeze elapsed (Keyframe { duration, stylers })
  = stylers <#> \(Styler { elements, fromProgress }) ->
      FrozenStyle
        { elements
        , style: fromProgress (elapsed / duration)
        }

frozenStyle_elements :: Traversal' FrozenStyle Element
frozenStyle_elements
  = _Newtype <<< prop (SProxy :: SProxy "elements")
             <<< traversed

frozenStyle_style :: Lens' FrozenStyle Style
frozenStyle_style
  = _Newtype <<< prop (SProxy :: SProxy "style")

---

newtype ContainedKeyframe
  = ContainedKeyframe
    { container :: Element
    , keyframe  :: Keyframe
    }

derive instance newtypeContainedKeyframe
  :: Newtype ContainedKeyframe _

containedKeyframe_container :: Lens' ContainedKeyframe Element
containedKeyframe_container
  = _Newtype <<< prop (SProxy :: SProxy "container")

containedKeyframe_keyframe :: Lens' ContainedKeyframe Keyframe
containedKeyframe_keyframe
  = _Newtype <<< prop (SProxy :: SProxy "keyframe")

---

newtype Scene
  = Scene
    { container :: Element
    , keyframes :: Array Keyframe
    }

derive instance newtypeScene :: Newtype Scene _

fromSceneSpec
  :: forall document eff m
   . IsParentNode document
  => MonadError String m
  => MonadEff (dom :: DOM | eff) m
  => document
  -> SceneSpec
  -> m Scene
fromSceneSpec document (SceneSpec { container, keyframes })
  = map Scene $
      { container: _, keyframes: _ }
        <$> container'
        <*> keyframes'

  where
    container' :: m Element
    container'
      = do
          element <- liftEff (querySelector (wrap container) document)

          case element of
            Nothing -> throwError ("Can't find " <> container <> "!")
            Just xs -> pure xs

    keyframes' :: m (Array Keyframe)
    keyframes'
      = traverse (fromKeyframeSpec document)
                 (fromMaybe [] (unwrap keyframes))

flatten :: Scene -> Array ContainedKeyframe
flatten (Scene { container, keyframes })
  = keyframes <#> \keyframe ->
      ContainedKeyframe
      { container
      , keyframe
      }

toChain
  :: Config
  -> Chain ContainedKeyframe
toChain (Config scenes)
  = fromFoldable $ scenes >>= flatten

scene_container :: Lens' Scene Element
scene_container = _Newtype <<< prop (SProxy :: SProxy "container")

scene_keyframe :: Traversal' Scene Keyframe
scene_keyframe = _Newtype <<< prop (SProxy :: SProxy "keyframes")
                          <<< traversed

---

newtype Config = Config (Array Scene)

derive instance newtypeConfig :: Newtype Config _

fromConfigSpec
  :: forall document eff m
   . IsParentNode document
  => MonadError String m
  => MonadEff (dom :: DOM | eff) m
  => document
  -> ConfigSpec
  -> m Config
fromConfigSpec doc (ConfigSpec config)
  = Config <$> traverse (fromSceneSpec doc) config

config_scenes :: Traversal' Config Scene
config_scenes = _Newtype <<< traversed
