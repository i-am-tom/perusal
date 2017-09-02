module Perusal.Config.Parser.Types where

import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)


opts :: Options
opts
  = defaultOptions
    { unwrapSingleConstructors = true
    }


newtype StyleSpec
  = StyleSpec
    { opacity    :: Tuple Number Number
    , scale      :: Tuple Number Number
    , rotate     :: Tuple Number Number
    , translateX :: Tuple Number Number
    , translateY :: Tuple Number Number
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
