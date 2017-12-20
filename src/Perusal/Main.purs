module Perusal.Main (fromJSON) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (alert, document)
import Data.Chain (Chain, left, right)
import Data.Either (Either(..))
import Data.Filterable (filtered)
import Data.Foreign (Foreign)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, singleton)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import FRP (FRP)
import FRP.Event (Event, mapAccum)
import FRP.Event.Time (debounceWith)
import Perusal.Config.Types as Types
import Perusal.Animation (withDelay)
import Perusal.HTML (init)
import Perusal.Navigation.Controls (direct, fromInput)
import Simple.JSON (read)

fromJSON
 :: forall eff
  . Foreign
 -> Eff (alert :: ALERT, dom :: DOM, frp :: FRP | eff) Unit
fromJSON json = do
    window'   <- window
    document' <- htmlDocumentToDocument <$> document window'

    let
      interpreted = case runExcept (read json) of
        Left  error  -> throwError (show (head error))
        Right result -> pure result

    parsed <- runExceptT (interpreted >>= Types.fromConfigSpec document')

    case parsed of
      Right config -> animate controls config
      Left  error  -> alert error window'
  where
    controls = { prev: singleton 37, next: singleton 39 }

animate
  :: forall eff
   . { prev :: Set Int, next :: Set Int }
  -> Types.Config
  -> Eff (frp :: FRP, dom :: DOM | eff) Unit
animate inputs config = init config *> do
  let frozen = withLast (map freezer delayed)
  void $ subscribe frozen render

  where
    movements = direct (reverser <<< left) right <$> fromInput inputs

    positions = filtered <<< mapAccum \mover chain ->
      map (Tuple chain Nothing) (map Just) (mover chain)

    reverser (Types.Keyframe k@{ stylers }) =
      Types.Keyframe (k { stylers = map Types.reverse stylers })

    prepare value@{ keyframe }
      = { value, period: keyframe.duration }

    delayed = withDelay (debounceWith (map prepare <<< positions)) movements
{-
    let
      movements :: forall a. Event (Chain a -> Maybe (Chain a /\ a))
      movements = direct left right <$> fromInput inputs

      positions :: _ -> Event (Chain _)
      positions moves
        = filtered
        $ mapAccum folder moves
        $ Types.toChain config

      delayed
        = withDelay
        $ debounceWith (map prepare <<< positions) (?f movements)

    -- void $ subscribe (withLast (map freezer delayed)) render
    pure unit

  where
    freezer { delay, value: { container, keyframe } } =
      { container, styles: Types.freeze delay keyframe }

    reverser (Types.Keyframe s@{ stylers })
      = Types.Keyframe (s { stylers = map Types.reverse stylers })

    prepare value@{ keyframe }
      = { value, period: keyframe.duration }

    folder mover chain = maybe (Tuple chain Nothing) (map Just) (mover chain)
-}
