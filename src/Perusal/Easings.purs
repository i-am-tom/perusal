module Perusal.Easings where

import Prelude

linear :: Number -> Number -> Number -> Number
linear start end progress = start + (progress * (end - start))
