module Base (
    profile
) where

import Linear (V2 (..), _y, zero)
import qualified Waterfall 
import Control.Lens ((^.))
import Data.Function (on)
import Splice (splice2D)

profile :: Double -> Double -> Double -> Double -> Waterfall.Path2D -> Waterfall.Path2D
profile rBase rNeck rCollar height skirting = 
    let 
        skirtingHeight = negate . uncurry ((-) `on` (^. _y)) . Waterfall.pathEndpoints $ skirting
        tweenHeight = height - 2 * skirtingHeight
    in Waterfall.pathFrom (V2 0 0)
        [ Waterfall.lineTo (V2 rBase 0)
        , splice2D skirting
        , Waterfall.bezierTo (V2 (rBase * 0.5) skirtingHeight) (V2 rNeck (skirtingHeight + tweenHeight/2)) (V2 rNeck (skirtingHeight + tweenHeight))
        , Waterfall.lineTo (V2 rCollar (skirtingHeight + tweenHeight))
        , splice2D skirting
        , Waterfall.lineTo (V2 0 height)
        ]

