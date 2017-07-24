module Perusal.Config.Parser where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, either)
import Perusal.Config.Parser.Types (decodeConfigSpec)
import Perusal.Config.Types (Config, fromConfigSpec)

parse :: forall eff m
       . MonadEff (dom :: DOM | eff) m
      => MonadError String m
      => String
      -> m Config
parse str = do
    htmldoc <- liftEff $ document =<< window
    let document = htmlDocumentToDocument htmldoc

    json <- lift $ jsonParser str
    spec <- lift $ decodeConfigSpec json

    fromConfigSpec document spec

  where lift :: forall a. Either String a -> m a
        lift = either throwError pure
