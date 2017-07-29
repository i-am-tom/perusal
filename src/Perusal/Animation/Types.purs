module Perusal.Animation.Types (AnimationFrame, fromConfig) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import DOM.Node.Types (Element)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tape (Tape(..), fromArray)
import Data.Tuple (Tuple)
import Perusal.Config.Types (Config, Milliseconds(..), Scene(..), Style)


type AnimationFrame =
  { container :: Element
  , duration :: Milliseconds
  , styles :: Array (Tuple (Array Element) Style)
  }


fromConfig :: forall m
            . MonadError String m
           => Config
           -> m (Tape AnimationFrame)
fromConfig config = case fromArray config of
  Just scenes -> pure (scenes >>= fromScene)
  Nothing     -> throwError "Where are your slides?!"


fromScene :: Scene -> Tape AnimationFrame
fromScene (Scene { container, frames }) =
    Tape [] initial (toFrame <<< unwrap <$> frames)
  where initial = { container, duration: Milliseconds 0, styles: [] }
        toFrame { duration, styles } = { container, duration, styles }
