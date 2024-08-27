module Split
( split
) where
import qualified Waterfall
import Linear (unit, (*^), _y, _z, V3 (..))
import Control.Lens ((^.))
import Data.Function ((&))
split :: Waterfall.Solid -> (Waterfall.Solid, Waterfall.Solid)
split s = 
    case Waterfall.axisAlignedBoundingBox s of 
        Nothing -> (Waterfall.nowhere, Waterfall.nowhere)
        Just aabb ->
            let d = (snd aabb - fst aabb) ^. _y / 2 
                mask = 
                    aabb 
                        & Waterfall.aabbToSolid 
                        & Waterfall.translate (d *^ unit _y)
                connector = 
                    Waterfall.centeredCube 
                        & Waterfall.translate (0.5 *^ unit _z)
                        & Waterfall.scale (V3 0.3 0.3 1)
                        & Waterfall.rotate (unit _z) (pi/4)
                s' = Waterfall.difference s connector
            in (s' `Waterfall.intersection` mask, s' `Waterfall.difference` mask)
    


