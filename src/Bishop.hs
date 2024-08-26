{-# LANGUAGE RecordWildCards #-}
module Bishop 
( topper
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), (^*), unit, _y)
import Topper (Args(..))

xSection :: Double -> Waterfall.Path2D
xSection radius = 
    let bobbleR = radius * 0.25
        h1 = radius * 3 - bobbleR
        hMid = h1 * 0.3
      in Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 (radius * 0.8) 0)
            , Waterfall.bezierTo (V2 (radius * 0.9) (hMid * 0.25)) (V2 radius (hMid * 0.75)) (V2 radius hMid )
            , Waterfall.bezierTo (V2 radius ((h1 + hMid) / 2 )) (V2 (radius * 0.25) h1) (V2 (bobbleR * 0.6) h1) 
            , Waterfall.arcViaRelative (V2 (bobbleR * 0.4) (bobbleR * 0.8)) (V2 (- (bobbleR * 0.6)) (bobbleR * 1.8))
            ]


cut :: Double -> Waterfall.Solid
cut r = Waterfall.translate (V3 0 0 r) $
            Waterfall.rotate (unit _y) (pi/4) $
                Waterfall.scale (V3 0.1 4 4 ^* r) $
                    Waterfall.translate (V3 0 0 0.5) Waterfall.centeredCube

topper :: Double -> Topper.Args -> Waterfall.Solid
topper radius Topper.Args {..} = topperSolidification (xSection radius) `Waterfall.difference` cut radius