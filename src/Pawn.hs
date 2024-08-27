{-# LANGUAGE RecordWildCards #-}
module Pawn (
    topper
) where

import qualified Waterfall
import Linear (zero, V2 (..))
import qualified Topper

xSection :: Double -> Double -> Waterfall.Path2D
xSection radius cutoff = Waterfall.translate2D (V2 0 (-cutoff)) $ Waterfall.arcVia zero (V2 radius radius) (V2 0 (2*radius))

topper :: Double -> Double -> Topper.Args -> Waterfall.Solid
topper cutoff radius Topper.Args {..} = topperSolidification (xSection radius cutoff)