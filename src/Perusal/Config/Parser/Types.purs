module Perusal.Config.Parser.Types where

import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.Index (readProp)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)


opts :: Options
opts
  = defaultOptions
    { unwrapSingleConstructors = true
    }

newtype SelectorSpec
  = SelectorSpec
    { from :: Number
    , to :: Number
    }

instance decodeSelectorSpec :: Decode SelectorSpec where
  decode x = do
    from <- decode =<< readProp "from" x
    to <- decode =<< readProp "to" x
    pure $ SelectorSpec {from, to}

newtype StyleSpec
  = StyleSpec
    { opacity    :: SelectorSpec
    , scale      :: SelectorSpec
    , rotate     :: SelectorSpec
    , translateX :: SelectorSpec
    , translateY :: SelectorSpec
    }


derive instance genericStyleSpec :: Generic StyleSpec _


instance decodeStyleSpec :: Decode StyleSpec where
  decode = genericDecode opts


newtype KeyframeSpec
  = KeyframeSpec
    { duration :: Number
    , styles   :: StrMap StyleSpec
    }


derive instance genericKeyframeSpec :: Generic KeyframeSpec _


instance decodeKeyframeSpec :: Decode KeyframeSpec where
  decode = genericDecode opts


newtype SceneSpec
  = SceneSpec
    { container :: String
    , keyframes :: Array KeyframeSpec
    }


derive instance genericSceneSpec :: Generic SceneSpec _


instance decodeSceneSpec :: Decode SceneSpec where
  decode = genericDecode opts


newtype ConfigSpec
  = ConfigSpec (Array SceneSpec)


derive instance genericConfigSpec :: Generic ConfigSpec _


instance decodeConfigSpec :: Decode ConfigSpec where
  decode = genericDecode opts
