module Perusal.Config.Types where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.ParentNode (class IsParentNode, querySelector, querySelectorAll)
import DOM.Node.NodeList (toArray)
import DOM.Node.Types (Element)
import Data.Chain (Chain, fromFoldable)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.StrMap (toUnfoldable)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Easing (polynomial)
import Perusal.Config.Parser.Types (ConfigSpec, KeyframeSpec(KeyframeSpec), SceneSpec(..), StyleSpec(StyleSpec))


-- | `Style` is pretty basic, as types go. All it does is present a
-- | set of values for a set of animation variables. Things to note:
-- | `rotate` is normalised so that `1` represents a full rotation.
-- | No one need worry about radians! The idea is that our animations
-- | will produce a stream of these objects, which we can then use for
-- | rendering simply by converting them to a CSS string and attaching
-- | them to their corresponding objects.
newtype Style = Style
  { opacity    :: Number
  , rotate     :: Number
  , scale      :: Number
  , translateX :: Number
  , translateY :: Number
  }


derive instance newtypeStyle :: Newtype Style _


-- | Of course, if you're _not_ going down the JSON route (and hooray
-- | for you!), you might like the feeling of *not* having to state
-- | the contents of every field explicitly. In which case, this is
-- | going to be one of your favourite things.
defaultStyle :: Style
defaultStyle = Style
  { opacity:    1.0
  , rotate:     0.0
  , scale:      1.0
  , translateX: 0.0
  , translateY: 0.0
  }


-- | *You*: _"I have a StyleSpec... how do I get a style?"_
-- | *Me*: `fromStyleSpec`, of course! This will take our StyleSpec
-- | and produce a function that, given progress (between 0 and 1),
-- | will produce the `Style` for that moment. We use a standard
-- | "quadratic" easing for JSON input (shout-out to Robert Penner),
-- | which can be swapped out for something way more interesting if we
-- | stay away from JSON. *Write PureScript*; I dare ya!
fromStyleSpec :: StyleSpec -> (Number -> Style)
fromStyleSpec (StyleSpec ss) progress =
  Style
    { opacity:    ss.opacity    `ease` progress
    , rotate:     ss.rotate     `ease` progress
    , scale:      ss.scale      `ease` progress
    , translateX: ss.translateX `ease` progress
    , translateY: ss.translateY `ease` progress
    }
  where

    -- Convert a `StyleSpec` tuple to a polynomial easing.
    ease :: Tuple Number Number -> Number -> Number
    ease = uncurry (polynomial 2.0)


-- | A `Styler` is the bit in a `Keyframe` that indicates the set of
-- | elements to be animated, as well as the function with which to
-- | animate them. This is really here to make the type signatures a
-- | little less horrific...
type Styler = Tuple (Array Element) (Number -> Style)


-- | Of course, once we have set a `Styler` to a given `progress`, we
-- | then have a value that is indeed `Renderable`! _See?_ I don't
-- | just make these names up, y'know!
type Renderable = Tuple (Array Element) Style


-- | Styles are fun, but we need to *animate*! In order to do that, we
-- | need an animation `duration`, and a list of `Styler` values. As
-- | time passes, we use `duration` and that time to track *progress*,
-- | which we use for the `Styler` functions.
newtype Keyframe = Keyframe
  { duration  :: Milliseconds
  , styles    :: Array Styler
  }


derive instance newtypeKeyframe :: Newtype Keyframe _


-- | We have a `Keyframe` to produce! Firstly, we're going to need a
-- | `document` to use as a parent node. Panic not: we'll probably use
-- | the `document` object, and you'll never need be any the wiser! :D
fromKeyframeSpec :: forall document eff m
                  . IsParentNode document
                 => MonadError String m
                 => MonadEff (dom :: DOM | eff) m
                 => document
                 -> KeyframeSpec
                 -> m Keyframe
fromKeyframeSpec document (KeyframeSpec ks) =
  map Keyframe $ { duration: _, styles: _ }
    <$> duration
    <*> traverse prepare (toUnfoldable ks.styles)
  where

    duration :: m Milliseconds
    duration = if ks.duration < 0.0
      then throwError "Negative duration!"
      else pure (Milliseconds ks.duration)

    prepare :: Tuple String StyleSpec -> m Styler
    prepare (Tuple query style) = do
      nodes <- liftEff $ querySelectorAll (wrap query) document
                     >>= toArray

      case traverse fromNode nodes of
        Just elems -> pure $ Tuple elems (fromStyleSpec style)
        Nothing    -> throwError "Keyframes contain non-elements!"


-- | Reverse the animation functions within a keyframe. This is how we
-- | animate a backwards movement! Nothing too fancy, so don't dwell
-- | too much on this one!
reverse :: Keyframe -> Keyframe
reverse = lens %~ (_ <<< sub 1.0)
  where

    lens :: ((Number -> Style) -> Number -> Style)
         -> Keyframe
         -> Keyframe
    lens = _Newtype
       <<< prop (SProxy :: SProxy "styles")
       <<< traversed
       <<< traversed


-- | `Styler` values are fun, but, sooner or later, we'll want to fix
-- | them to a given value. To do this, we take the `Milliseconds`
-- | elapsed since our animation "started", and use that to calculate
-- | our `progress`. If the animation is running, we can return a
-- | `Renderable`; if it isn't, we panic, freak out, and run away.
freeze :: Milliseconds -> Keyframe -> Maybe (Array Renderable)
freeze elapsed (Keyframe { duration, styles }) =
  guard (between 0.0 1.0 progress) $> map (_ <@> progress) styles
  where

    progress :: Number
    progress = unwrap elapsed / unwrap duration


-- | Scenes are like your traditional presentation "slides": each one
-- | has a set of animation keyframes and we play them in order as the
-- | user clicks back and forth. Oh, y'all wanted a *twist*? You can
-- | include animations within keyframes that happen *outside* the
-- | scenes. This is pretty useful if you want animations to run over
-- | all your slides and don't like duplicating logic.
newtype Scene = Scene
  { container :: Element
  , keyframes :: Array Keyframe
  }


derive instance newtypeScene :: Newtype Scene _


-- | To produce a `Scene` from a `SceneSpec`, we're going to need to
-- | convert the `KeyframeSpec`s to `Keyframe`s _and_ get the element
-- | selected by `container`. It's all a bit... `Applicative`.
fromSceneSpec :: forall document eff m
                  . IsParentNode document
                 => MonadError String m
                 => MonadEff (dom :: DOM | eff) m
                 => document
                 -> SceneSpec
                 -> m Scene
fromSceneSpec document (SceneSpec { container, keyframes }) =
  map Scene $ { container: _, keyframes: _ } <$> container'
                                             <*> keyframes'
  where
    liftMaybe :: String -> Maybe ~> m
    liftMaybe error = maybe (throwError error) pure

    container' :: m Element
    container' = liftEff (querySelector (wrap container) document)
      >>= liftMaybe ("Can't find " <> container <> "!")

    keyframes' :: m (Array Keyframe)
    keyframes' = traverse (fromKeyframeSpec document) keyframes


-- | A container element and its frozen animation properties.
type RenderFrame = Tuple Element (Array Renderable)


-- | Take a `Scene`, produce a list of `Tuple Element Keyframe`. This
-- | is how we turn a `Config` into something we can actually animate:
-- | produce this tuple list, then put that into a `Chain`. All we
-- | then need is a way of fixing the keyframe to a given progress...
-- | `freeze`-ing it, if you will... and we're away!
flatten :: Scene -> Array (Tuple Element Keyframe)
flatten (Scene { container, keyframes }) =
  Tuple container <$> keyframes


-- | Nice `Config` you got there. It'd look better as a `Chain`,
-- | though, wouldn't it? _Shh_, tell no one, but this function will
-- | sort that right out. This is a way more useful function than just
-- | `flatten` on its own.
toChain :: Config -> Chain (Tuple Element Keyframe)
toChain (Config scenes) = fromFoldable $ scenes >>= flatten


-- | A `Config` for a presentation is just an array of `Scene`s. I
-- | can't understate how unremarkable this is.
newtype Config = Config (Array Scene)


derive instance newtypeConfig :: Newtype Config _


-- | Oooh wow, you've gone *full JSON*, haven't you? _Well_, as luck
-- | would have it, this function is the last we have in stock. This
-- | function will fully translate a `ConfigSpec` into `Config`. It's
-- | pretty neat, right?
fromConfigSpec :: forall document eff m
                . IsParentNode document
               => MonadError String m
               => MonadEff (dom :: DOM | eff) m
               => document
               -> ConfigSpec
               -> m Config
fromConfigSpec doc = map wrap
  <<< traverse (fromSceneSpec doc)
  <<< unwrap
