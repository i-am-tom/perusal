module Perusal.Main (fromJSON) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Reader.Trans (runReaderT)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (alert, document)
import DOM.Node.Types (Element)
import Data.Chain (Chain, left, right')
import Data.Either (Either(..))
import Data.Filterable (filtered)
import Data.Foreign (Foreign)
import Data.Lens ((%~))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set, singleton)
import Data.Tuple (Tuple(..))
import FRP (FRP)
import FRP.Event (Event, mapAccum, subscribe, withLast)
import Perusal.Config.Parser.Types (ConfigSpec)
import Perusal.Config.Types (Config, ContainedKeyframe(..), FrozenStyle, containedKeyframe_keyframe, freeze, fromConfigSpec, reverse, toChain)
import Perusal.HTML (render)
import Perusal.Navigation.Controls (fromDirection, fromInput, withBlocking, withDelay)
import Simple.JSON (read)

-- | Run a slideshow from a "JSON object". Worth noting that what this
-- _really_ means is that the user provides the output of a call to
-- JSON.parse.
fromJSON
  :: forall eff.
     Foreign
  -> Eff ( alert :: ALERT
         , dom   :: DOM
         , frp   :: FRP
         | eff
         ) Unit
fromJSON json
  = do
      window'   <- window
      document' <- htmlDocumentToDocument <$> document window'

      let
        interpreted
          :: forall m.
             MonadError String m
          => m ConfigSpec
        interpreted
          = case runExcept (read json) of
              Left  error  -> throwError (show (head error))
              Right result -> pure result

      -- From ConfigSpec comes Config, all being well.
      parsed <- runExceptT (interpreted >>= fromConfigSpec document')

      case parsed of
        Right config -> runReaderT (animate config) controls
        Left  error  -> alert error window'
  where

    -- The starter set of keyboard controls for a presentation.
    controls :: { prev :: Set Int, next :: Set Int }
    controls
      = { prev: singleton 37
        , next: singleton 39
        }

-- | Given a Config object, bootstrap the presentation and begin
-- listening for key presses.
animate
  :: forall eff m.
     MonadEff
       ( frp :: FRP
       , dom :: DOM
       | eff
       ) m
  => MonadReader
       { prev :: Set Int
       , next :: Set Int
       } m
  => Config
  -> m Unit
animate config
  = do
      inputs <- fromInput

      let
        -- The inputs are mapped to movement functions that may or may
        -- not work.
        movements
          :: Event (Chain ContainedKeyframe
                      -> Maybe (Tuple (Chain ContainedKeyframe)
                                      ContainedKeyframe))
        movements
          = fromDirection (left reverser) right' <$> inputs

        -- Apply those movements to the config chain, and throw away
        -- any that fail. Now, we have the set of stages to render.
        fixed :: Event ContainedKeyframe
        fixed
          = withBlocking movements \movement -> prepare
              <$> filtered (mapAccum folder movement (toChain config))

        -- We can add "delay" to them to calculate progression through
        -- an animation.
        delayed :: Event
                     { delay :: Number
                     , value :: ContainedKeyframe
                     }
        delayed = withDelay fixed

        -- With this delay, we can "freeze" the styler to the state
        -- it should have for this moment in the animation.
        frozen :: Event
                    { container :: Element
                    , styles :: Array FrozenStyle
                    }
        frozen
          = delayed <#> \{ delay, value } ->
              let ContainedKeyframe { container, keyframe } = value
              in { container, styles: freeze delay keyframe }

      -- Add the renderer, throw away the canceller.
      liftEff (void (subscribe (withLast frozen) render))

    where
      -- When we move left, we "flip" the frame to animate backwards.
      reverser :: ContainedKeyframe -> ContainedKeyframe
      reverser = containedKeyframe_keyframe %~ reverse

      -- The "duration" of the frame is how long we'll block for.
      prepare
        :: ContainedKeyframe
        -> { period :: Number
           , value  :: ContainedKeyframe
           }
      prepare value@(ContainedKeyframe { keyframe })
        = { value
          , period: (unwrap keyframe).duration
          }

      -- Apply the movement functions and carry the result along the
      -- stream. Here's how we'll save "state".
      folder
        :: (Chain ContainedKeyframe
              -> Maybe (Tuple (Chain ContainedKeyframe)
                              ContainedKeyframe))
        -> Chain ContainedKeyframe
        -> Tuple (Chain ContainedKeyframe) (Maybe ContainedKeyframe)
      folder mover chain
        = maybe (Tuple chain Nothing) (map Just) (mover chain)
