module Perusal.Config.Parser.Types where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Simple.JSON (class ReadForeign)

newtype Transition
  = Transition
      { to   :: Number
      , from :: Number
      }

derive instance newtypeTransition :: Newtype Transition _
derive newtype instance readForeignTransition
  :: ReadForeign Transition

newtype Style
  = Style
      { opacity    :: NullOrUndefined Transition
      , rotate     :: NullOrUndefined Transition
      , scale      :: NullOrUndefined Transition
      , translateX :: NullOrUndefined Transition
      , translateY :: NullOrUndefined Transition
      }

derive instance newtypeStyle :: Newtype Style _
derive newtype instance readForeignStyle
  :: ReadForeign Style

newtype Keyframe
  = Keyframe
      { duration :: Number
      , styles   :: StrMap Style
      }

derive instance newtypeKeyframe :: Newtype Keyframe _
derive newtype instance readForeignKeyframe
  :: ReadForeign Keyframe

newtype Scene
  = Scene
    { container :: String
    , keyframes :: NullOrUndefined (Array Keyframe)
    }

derive instance newtypeScene :: Newtype Scene _
derive newtype instance readForeignScene
  :: ReadForeign Scene

newtype Config
  = Config (Array Scene)

derive instance newtypeConfig :: Newtype Config _
derive newtype instance readForeignConfig
  :: ReadForeign Config

