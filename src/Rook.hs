{-# LANGUAGE RecordWildCards #-}
module Rook
( topper
, polygonalCrenellations 
, radialCrenellations
) where


import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), unit, _z, _x, _y, (*^))
import Topper (Args(..))
import Data.Function ((&))

xSection :: Double -> Waterfall.Path2D
xSection radius = 
    let h = radius * 2
        hi = radius * 1.4
        ri = radius * 0.8
      in 
            Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 radius 0)
            , Waterfall.lineTo (V2 radius h)
            , Waterfall.lineTo (V2 ri h)
            , Waterfall.lineTo (V2 ri hi)
            , Waterfall.lineTo (V2 0 hi)
            ]


radialCrenellations :: Int -> Double -> Waterfall.Solid
radialCrenellations n r =
    let t = r * 0.3 
        beam = Waterfall.scale (V3 t (r*4) (t*2)) $ Waterfall.translate (V3 0 0.5 0) Waterfall.centeredCube
        angle = (2 * pi) / fromIntegral n
        in Waterfall.translate (V3 0 0 (r * 2)). mconcat . take n . iterate (Waterfall.rotate (unit _z) angle) $ beam
        
polygonalCrenellations :: Int -> Int -> Double -> Double -> Waterfall.Solid
polygonalCrenellations nSides cutsPerSide scaleFactor radius =
  let angle = 2 * pi / fromIntegral nSides
      sideW = scaleFactor * 2 * radius * tan (angle/2)
      cutW = sideW / (2 * fromIntegral cutsPerSide + 1)
      oneCut = 
        mconcat $
            [ let x = fromIntegral i * 2 * cutW - sideW/2
               in Waterfall.centeredCube 
                    & Waterfall.translate (V3 (-0.5) 0.5 0)
                    & Waterfall.scale (V3 cutW (radius * 2) (cutW * 2)) 
                    & Waterfall.translate (V3 x 0 (radius* 2)) 
                | i <- [1 .. cutsPerSide]
            ]
      allCuts = mconcat . take nSides . iterate (Waterfall.rotate (unit _z) angle) $ oneCut
  in allCuts


topper :: (Double -> Waterfall.Solid) -> Double -> Topper.Args -> Waterfall.Solid
topper crenellations radius Topper.Args {..} = topperSolidification (xSection radius) `Waterfall.difference` crenellations radius