module Perusal.Config.Parser.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Either (Either)
import Data.Map (Map, fromFoldable)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))

newtype StyleSpec = StyleSpec
  { opacity    :: Tuple Number Number
  , scale      :: Tuple Number Number
  , rotate     :: Tuple Number Number
  , translateX :: Tuple Number Number
  , translateY :: Tuple Number Number
  }

derive instance newtypeStyleSpec :: Newtype StyleSpec _

instance decodeJsonStyleSpec :: DecodeJson StyleSpec where
  decodeJson str = do
    json <- decodeJson str

    let double x = pure (Tuple x x)
    opacity    <- json .? "opacity"    <|> double 1.0
    rotate     <- json .? "rotate"     <|> double 0.0
    scale      <- json .? "scale"      <|> double 1.0
    translateX <- json .? "translateX" <|> double 0.0
    translateY <- json .? "translateY" <|> double 0.0

    pure $ StyleSpec
      { opacity
      , scale
      , rotate
      , translateX
      , translateY
      }


newtype KeyframeSpec = KeyframeSpec
  { duration :: Int
  , styles   :: Map String StyleSpec
  }

derive instance newtypeKeyframeSpec :: Newtype KeyframeSpec _

instance decodeJsonKeyframeSpec :: DecodeJson KeyframeSpec where
  decodeJson str = do
    json     <- decodeJson str
    duration <- json .? "duration" <|> pure 1000
    styles   <- json .? "styles"   <|> pure (fromFoldable [])

    pure $ KeyframeSpec { duration, styles }


newtype SceneSpec = SceneSpec
  { container :: String
  , frames    :: Array KeyframeSpec
  }

derive instance newtypeSceneSpec :: Newtype SceneSpec _

instance decodeJsonSceneSpec :: DecodeJson SceneSpec where
  decodeJson str = do
    json      <- decodeJson str
    container <- json .? "container"
    frames    <- json .? "frames" <|> pure []

    pure $ SceneSpec { container, frames }


type ConfigSpec = Array SceneSpec

decodeConfigSpec :: Json -> Either String ConfigSpec
decodeConfigSpec = decodeJson >=> traverse decodeJson
