module Polygonize 
( polygonize 
) where

import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), unit, _z)

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
        prism = 
            Waterfall.rotate (unit _z) (pi/2) $
            Waterfall.rotate (V3 1 0 0) (pi/2) $ 
                Waterfall.translate (V3 0 0 (-5)) $ 
                    Waterfall.prism 10 (Waterfall.fromPath (Waterfall.closeLoop path))
        maskedPrism = Waterfall.intersection mask prism
    in   mconcat . take n . iterate (Waterfall.rotate (unit _z) angle) $ maskedPrism 