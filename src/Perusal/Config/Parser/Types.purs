module Perusal.Config.Parser.Types where

import Prelude hiding (const)

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))


-- | A `StyleSpec` is the building block of animations in your JSON!
-- | The `Tuple` values indicate a `start` and `end` value for the
-- | properties, which is rather neat. Oh, one more thing: rotation is
-- | normalised such that `1.0` is a 2r/360º rotation. _C'est tout!_
newtype StyleSpec = StyleSpec
  { opacity    :: Tuple Number Number
  , scale      :: Tuple Number Number
  , rotate     :: Tuple Number Number
  , translateX :: Tuple Number Number
  , translateY :: Tuple Number Number
  }


derive instance newtypeStyleSpec :: Newtype StyleSpec _


-- | Decode a JSON blob into a `StyleSpec`. Well, *try* to, anyway. It
-- | could all go horribly wrong, of course. _Luckily_, `Argonaut` has
-- | some pretty great errors, so it'll let you know.
instance decodeJsonStyleSpec :: DecodeJson StyleSpec where
  decodeJson = decodeJson >=> \json -> map StyleSpec $
    { opacity: _, rotate: _, scale: _, translateX: _, translateY: _ }
      <$> ( (json .? "opacity"    >>= decodeJson) <|> fallback 1.0 )
      <*> ( (json .? "rotate"     >>= decodeJson) <|> fallback 0.0 )
      <*> ( (json .? "scale"      >>= decodeJson) <|> fallback 1.0 )
      <*> ( (json .? "translateX" >>= decodeJson) <|> fallback 0.0 )
      <*> ( (json .? "translateY" >>= decodeJson) <|> fallback 0.0 )
    where

      fallback :: forall a f. Applicative f => a -> f (Tuple a a)
      fallback x = pure $ Tuple x x

-- | A `KeyframeSpec` defines a `duration` for which to animate the
-- | `StrMap StyleSpec`. The `String` key is a query selector for the
-- | element being animated. Nothing too frightening :)
newtype KeyframeSpec = KeyframeSpec
  { duration :: Int
  , styles   :: StrMap StyleSpec
  }


derive instance newtypeKeyframeSpec :: Newtype KeyframeSpec _


-- | This one's a lottle easier to decode as we don't have any data
-- | that we wouldn't mind people skipping. If you can't give us a
-- | duration and a set of styles to animate, you probably don't want
-- | a `Keyframe`!
instance decodeJsonKeyframeSpec :: DecodeJson KeyframeSpec where
  decodeJson = decodeJson >=> \json -> map KeyframeSpec $
    { duration: _, styles: _ }
      <$> json .? "duration"
      <*> json .? "styles"


-- | The `SceneSpec` is kinda the same thing as the "slide" in your
-- | Favourite Presentation-Editing Software™. The `container` tells
-- | us how to find the element that acts as our "slide", and the
-- | `keyframes` are the animations that will happen on the slide. We
-- | do have one _slight_ advantage over the usual software, though...
-- | your animations aren't confined to the slide! You can have an
-- | element independent of your slides that can be animated at any
-- | point. That's pretty neat, I think.
newtype SceneSpec = SceneSpec
  { container :: String
  , keyframes :: Array KeyframeSpec
  }


derive instance newtypeSceneSpec :: Newtype SceneSpec _


-- | So, we can decode a JSON blob into some `SceneSpec` type without
-- | much trouble. Maybe we want boring slides? No problem; we can
-- | simply use an empty `keyframes` array, or omit it entirely!
instance decodeJsonSceneSpec :: DecodeJson SceneSpec where
  decodeJson = decodeJson >=> \json -> map SceneSpec $
    { container: _, keyframes: _ }
      <$>  (json .? "container" >>=          decodeJson)
      <*> ((json .? "keyframes" >>= traverse decodeJson) <|> pure [])


-- | The entire configuration for a slideshow is really just an array
-- | of `SceneSpec` values. A presentation is a set of slides to show.
-- | A show of slides. A *slideshow*. Sedimentary, my dear Watson.
newtype ConfigSpec = ConfigSpec (Array SceneSpec)


derive instance newtypeConfigSpec :: Newtype ConfigSpec _


-- | Oh, this little thing? Pretty snazzy, right? Decode a JSON array
-- | into an `Array SceneSpec`, then wrap it up in a `ConfigSpec`.
-- | *Bosh*.
instance decodeJsonConfigSpec :: DecodeJson ConfigSpec where
  decodeJson :: Json -> Either String ConfigSpec
  decodeJson = map ConfigSpec <<< decodeJson
