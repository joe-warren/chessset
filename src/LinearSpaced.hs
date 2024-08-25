module LinearSpaced
( linearSpaced
) where
import qualified Waterfall
import Linear (_x,  unit, (*^))
import Control.Lens ((^.))
import Data.Foldable (foldl')

linearSpaced :: Double -> [Waterfall.Solid] -> Waterfall.Solid
linearSpaced space = 
    let go (xOff, rest) shape = 
            case Waterfall.axisAlignedBoundingBox shape of 
                Just (lo, hi) -> 
                    let width = (hi-lo) ^. _x
                        displacement = xOff - lo ^. _x
                        displaced = Waterfall.translate (displacement *^ unit _x) shape
                    in (xOff + space + width, rest <> displaced)
                Nothing -> (xOff, rest)
    in snd . foldl' go (0, Waterfall.nowhere)
