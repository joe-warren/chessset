{-# LANGUAGE RecordWildCards #-}
module King
( topper
) where


import qualified Waterfall
import Linear (zero, V2(..), V3 (..))
import qualified Topper

xSection :: Double -> Waterfall.Path2D
xSection radius = 
    let h1 = radius * 1.5
        hMid = h1 * 0.75
      in Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 (radius * 0.8) 0)
            , Waterfall.bezierTo (V2 (radius * 0.9) (hMid * 0.25)) (V2 radius (hMid * 0.75)) (V2 radius hMid )
            , Waterfall.bezierTo (V2 radius ((h1 + hMid) / 2 )) (V2 (radius * 0.5) h1) (V2 0 h1) 
            ]

cross :: Double -> Double -> Double -> Waterfall.Solid
cross l1 l2 t = 
    let b1 =  Waterfall.scale (V3 l2 t t) Waterfall.centeredCube
        b2 = Waterfall.translate (V3 0 0 ((l2 - l1)/2)) $ Waterfall.scale (V3 t t l1) Waterfall.centeredCube
     in Waterfall.translate (V3 0 0 (l1/2)) (b1 <> b2)


topper :: Double -> Topper.Args -> Waterfall.Solid
topper radius Topper.Args {..} = (topperSolidification $ xSection radius) <> (Waterfall.translate (V3 0 0 (radius * 1.45)) $ cross radius (radius * 0.75) (radius * 0.3))