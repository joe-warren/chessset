module TextTopper
( textTopper
) where
    
import qualified Waterfall
import qualified Topper
import Linear (unit, _x, _y, _z, V3 (..), (*^))
import Control.Lens ((^.))
import Data.Function ((&))

textTopper :: Waterfall.Font -> String -> Double -> Topper.Args -> Waterfall.Solid
textTopper font text radius _args = 
    let prism =  Waterfall.text font text          
                & Waterfall.prism radius 
                & Waterfall.translate (- ((radius / 2) *^ unit _z ))
                & Waterfall.rotate (unit _x) (pi/2)
     in case Waterfall.axisAlignedBoundingBox prism of 
            Nothing -> Waterfall.nowhere
            Just (lo, hi) -> 
                let w = (hi - lo) ^. _x
                    h = (hi-lo) ^. _z
                    dx = -(((hi + lo)/2) ^. _x)
                    dz = - (lo ^. _z)
                    s = 2*radius / max w h
                 in prism  
                        & Waterfall.translate (V3 dx 0 dz) 
                        & Waterfall.scale (V3 s 1 s)