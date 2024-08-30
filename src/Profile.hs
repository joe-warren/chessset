module Profile 
( Segment (..)
, makeProfile
) where

import Linear (V2 (..), _y, zero)
import qualified Waterfall 
import Control.Lens ((^.))
import Data.Function (on)
import Splice (splice2D)
import Data.Either (partitionEithers)
import Data.Bifunctor (bimap)

data Segment = FixedSizeSegment Waterfall.Path2D | VariableSizeSegment Waterfall.Path2D

makeProfile :: [Segment] -> Double -> Waterfall.Path2D
makeProfile segments targetHeight =
    let pathHeight = negate . uncurry ((-) `on` (^. _y)) . Waterfall.pathEndpoints
        segmentToEither s = 
            case s of
                (FixedSizeSegment p) -> Left p
                (VariableSizeSegment p) -> Right p
        sumHeights = sum . fmap pathHeight 
        (fixedHeight, variableHeight) = bimap sumHeights sumHeights . partitionEithers . fmap segmentToEither $ segments
        targetVariableHeight = targetHeight - fixedHeight
        scale = targetVariableHeight / variableHeight
        scaledPath s = case s of 
            (FixedSizeSegment p) -> p 
            (VariableSizeSegment p) -> Waterfall.scale2D (V2 1 scale) p
    in Waterfall.pathFrom zero (splice2D . scaledPath <$> segments)
