module Rook
( topper
) where


import qualified Waterfall
import Linear (zero, V2 (..), V3 (..), unit, _z)

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


cut :: Double -> Int -> Waterfall.Solid
cut r n =
    let t = r * 0.3 
        beam = Waterfall.scale (V3 t (r*4) (t*2)) $ Waterfall.translate (V3 0 0.5 0) Waterfall.centeredCube
        angle = (2 * pi) / fromIntegral n
        in Waterfall.translate (V3 0 0 (r * 2)). mconcat . take n . iterate (Waterfall.rotate (unit _z) angle) $ beam
        

topper :: Double -> Waterfall.Solid
topper radius = (Waterfall.revolution $ xSection radius) `Waterfall.difference` cut radius 7