module Pawn (
    topper
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), _z)

xSection :: Double -> Double -> Waterfall.Path2D
xSection radius cutoff = Waterfall.translate2D (V2 0 (-cutoff)) $ Waterfall.arcVia zero (V2 radius radius) (V2 0 (2*radius))

topper :: Double -> Double -> Waterfall.Solid
topper cutoff radius = 
    let mask = Waterfall.uScale (2 * radius) $ Waterfall.translate (V3 0 0 0.5) $ Waterfall.centeredCube
    in (Waterfall.revolution $ xSection radius cutoff)