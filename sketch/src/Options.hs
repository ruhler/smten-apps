
-- | Representation of command line and pragma options.
module Options (Options(..), defaultOptions) where

import Smten.Prelude

data Options = Options {
  bnd_cbits :: Int,
  bnd_inbits :: Int,
  bnd_unroll_amnt :: Int,
  bnd_inline_amnt :: Int
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    bnd_cbits = 5,
    bnd_inbits = 5,
    bnd_unroll_amnt = 8,
    bnd_inline_amnt = 5
}

