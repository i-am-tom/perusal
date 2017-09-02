module Perusal.Config.Parser.Types where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Lens (Lens', Traversal', _Just, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))
import Prelude ((<<<))
import Simple.JSON (class ReadForeign)

---

type Transition
  = { to   :: Number
    , from :: Number
    }

transition_to :: Lens' Transition Number
transition_to = prop (SProxy :: SProxy "to")

transition_from :: Lens' Transition Number
transition_from = prop (SProxy :: SProxy "from")

---

newtype StyleSpec
  = StyleSpec
    { opacity    :: NullOrUndefined Transition
    , rotate     :: NullOrUndefined Transition
    , scale      :: NullOrUndefined Transition
    , translateX :: NullOrUndefined Transition
    , translateY :: NullOrUndefined Transition
    }

derive instance newtypeStyleSpec :: Newtype StyleSpec _
derive newtype instance readForeignStyleSpec
  :: ReadForeign StyleSpec

styleSpec_opacity :: Traversal' StyleSpec Transition
styleSpec_opacity
    = _Newtype
  <<< prop (SProxy :: SProxy "opacity")
  <<< _Newtype
  <<< _Just

styleSpec_rotate :: Traversal' StyleSpec Transition
styleSpec_rotate
    = _Newtype
  <<< prop (SProxy :: SProxy "rotate")
  <<< _Newtype
  <<< _Just

styleSpec_scale :: Traversal' StyleSpec Transition
styleSpec_scale
    = _Newtype
  <<< prop (SProxy :: SProxy "scale")
  <<< _Newtype
  <<< _Just

styleSpec_translateX :: Traversal' StyleSpec Transition
styleSpec_translateX
    = _Newtype
  <<< prop (SProxy :: SProxy "translateX")
  <<< _Newtype
  <<< _Just

styleSpec_translateY :: Traversal' StyleSpec Transition
styleSpec_translateY
    = _Newtype
  <<< prop (SProxy :: SProxy "translateY")
  <<< _Newtype
  <<< _Just

---

newtype KeyframeSpec
  = KeyframeSpec
    { duration :: Number
    , styles   :: StrMap StyleSpec
    }

derive instance newtypeKeyframeSpec :: Newtype KeyframeSpec _
derive newtype instance readForeignKeyframeSpec
  :: ReadForeign KeyframeSpec

keyframeSpec_duration :: Lens' KeyframeSpec Number
keyframeSpec_duration
    = _Newtype
  <<< prop (SProxy :: SProxy "duration")

keyframeSpec_styles :: Traversal' KeyframeSpec StyleSpec
keyframeSpec_styles
    = _Newtype
  <<< prop (SProxy :: SProxy   "styles")
  <<< traversed

---

newtype SceneSpec
  = SceneSpec
    { container :: String
    , keyframes :: NullOrUndefined (Array KeyframeSpec)
    }

derive instance newtypeSceneSpec :: Newtype SceneSpec _
derive newtype instance readForeignSceneSpec
  :: ReadForeign SceneSpec

sceneSpec_container :: Lens' SceneSpec String
sceneSpec_container
    =_Newtype
  <<< prop (SProxy :: SProxy "container")

sceneSpec_keyframe :: Traversal' SceneSpec KeyframeSpec
sceneSpec_keyframe
    = _Newtype
  <<< prop (SProxy :: SProxy "keyframes")
  <<< _Newtype
  <<< _Just
  <<< traversed

---

newtype ConfigSpec
  = ConfigSpec (Array SceneSpec)

derive instance newtypeConfigSpec :: Newtype ConfigSpec _
derive newtype instance readForeignConfigSpec
  :: ReadForeign ConfigSpec

configSpec :: Traversal' ConfigSpec SceneSpec
configSpec = _Newtype <<< traversed
