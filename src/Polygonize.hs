module Polygonize 
( polygonize
, sideLength
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), unit, _z)

scaleFactor :: Double -> Double 
scaleFactor angle = (1 + cos (angle/2)) / 2

sideLength :: Int -> Double -> Double 
sideLength n radius = 
    let angle = 2 * pi / fromIntegral n
    in scaleFactor angle * 2 * radius * tan (angle/2)
    
polygonize :: Int -> Waterfall.Path2D -> Waterfall.Solid
polygonize n path = 
    let angle = 2 * pi / fromIntegral n
        spoke = V2 0 10
        maskPath = Waterfall.pathFrom2D zero 
            [ Waterfall.lineTo (Waterfall.rotate2D (-angle / 2 ) spoke)
            , Waterfall.arcViaTo spoke (Waterfall.rotate2D (angle/2) spoke)
            , Waterfall.lineTo zero
            ]
        mask = Waterfall.prism 10 (Waterfall.fromPath maskPath)
        scalePath = Waterfall.scale2D (V2 (scaleFactor angle) 1)
        prism = 
            Waterfall.rotate (unit _z) (pi/2) $
            Waterfall.rotate (V3 1 0 0) (pi/2) $ 
                Waterfall.translate (V3 0 0 (-5)) $ 
                    Waterfall.prism 10 (Waterfall.fromPath . scalePath $ (Waterfall.closeLoop path))
        maskedPrism = Waterfall.intersection mask prism
    in  
        Waterfall.rotate (unit _z) (pi/2) . mconcat . take n . iterate (Waterfall.rotate (unit _z) angle) $ maskedPrism 