{-# LANGUAGE RecordWildCards #-}
module Queen 
( topper
, simpleTopper
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), unit, _x, _z)
import qualified Topper

xSection :: Double -> Waterfall.Path2D
xSection radius = 
    let bobbleR = radius * 0.25
        h1 = radius * 1.5 - bobbleR
        hMid = h1 * 0.75
      in Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 (radius * 0.8) 0)
            , Waterfall.bezierTo (V2 (radius * 0.9) (hMid * 0.25)) (V2 radius (hMid * 0.75)) (V2 radius hMid )
            , Waterfall.bezierTo (V2 radius ((h1 + hMid) / 2 )) (V2 (radius * 0.5) h1) (V2 (bobbleR * 0.6) h1) 
            , Waterfall.arcViaRelative (V2 (bobbleR * 0.4) (bobbleR * 0.8)) (V2 (-bobbleR * 0.6) (bobbleR * 1.8))
            ]
            
xSectionOuter :: Double -> Waterfall.Path2D
xSectionOuter radius = 
    let bobbleR = radius * 0.25
        h1 = radius * 1.5 - bobbleR
        hMid = h1 * 0.75
      in Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 (radius * 0.8) 0)
            , Waterfall.bezierTo (V2 (radius * 0.9) (hMid * 0.25)) (V2 radius (hMid * 0.75)) (V2 radius hMid )
            , Waterfall.lineRelative (V2 0 (h1 - hMid))
            , Waterfall.lineRelative (V2 (-0.1 * radius) 0)
            , Waterfall.bezierTo  (V2 (0.8 * radius) (h1 * 0.75)) (V2 (radius * 0.7) (h1 * 0.25)) (V2 (radius * 0.6) (h1 * 0.1))
            , Waterfall.lineTo (V2 0 (h1 * 0.1))
            ]

cuts :: Int -> Double -> Waterfall.Solid
cuts n radius = 
  let angle = 2 * pi / fromIntegral n
      cutW = 0.8 * radius * sin (angle/2)
      oneCut = 
        Waterfall.translate (V3 0 0 (radius * 1.25)) $
        Waterfall.rotate (unit _x) (pi/2) $
        Waterfall.scale (V3 cutW cutW (radius * 2)) Waterfall.unitCylinder
      allCuts = mconcat . take n . iterate (Waterfall.rotate (unit _z) angle) $ oneCut
  in allCuts
      


simpleTopper :: Double -> Topper.Args -> Waterfall.Solid
simpleTopper radius Topper.Args {..} = 
  (topperSolidification $ xSection radius) 

topper :: Int -> Double -> Topper.Args -> Waterfall.Solid
topper cutCount radius Topper.Args {..} = 
  (topperSolidification $ xSection radius) 
  <> ((topperSolidification $ xSectionOuter radius) `Waterfall.difference` (cuts cutCount radius))