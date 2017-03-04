module Data.Config where

import Prelude ((<<<), (>=>))
import Data.Argonaut.Core (Json, toArray, toString)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)

-- | Interpret some JSON into Perusal config. Currently,
-- | this is just the string list of frame selectors.
read :: Json -> Array String
read = fromMaybe [] <<< parse
  where parse = toArray >=> traverse toString
