module Pawn (
    pawn
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), _z)
import Control.Lens ((^.))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Base

xSection :: Double -> Double -> Waterfall.Path2D
xSection radius cutoff = Waterfall.translate2D (V2 0 (-cutoff)) $ Waterfall.arcVia zero (V2 radius radius) (V2 0 (2*radius))

topper :: Double -> Double -> Waterfall.Solid
topper radius cutoff = 
    let mask = Waterfall.uScale (2 * radius) $ Waterfall.translate (V3 0 0 0.5) $ Waterfall.centeredCube
    in (Waterfall.revolution $ xSection radius cutoff)

pawn :: Double -> Double -> Double -> Double -> Double -> Waterfall.Path2D -> Waterfall.Solid
pawn baseR neckR collarR topperR height skirting =
    let top = topper topperR 0.1
        topH =  fromMaybe 0 . fmap (negate . uncurry ((-) `on` (^. _z))) . Waterfall.axisAlignedBoundingBox $ top  
        baseH = height - topH
        baseProfile = Base.profile baseR neckR collarR baseH skirting
        base = Waterfall.revolution baseProfile
    in Waterfall.translate (V3 0 0 baseH) top <> base