module Skirting
( classicSkirting
, crenellatedSkirting
) where
import qualified Waterfall
import Linear (zero, V2 (..))

classicSkirting :: Waterfall.Path2D
classicSkirting = Waterfall.pathFrom zero
    [ Waterfall.lineRelative2D (V2 0.05 0.025)
    , Waterfall.lineRelative2D (V2 (-0.05) 0.075)
    , Waterfall.arcViaRelative2D (V2 0.05 0.05) (V2 0 0.1)
    ]

crenellatedSkirting :: Int -> Waterfall.Path2D
crenellatedSkirting n = 
    let oneCrenelation = Waterfall.pathFrom zero
            [ Waterfall.lineRelative2D (V2 0.05 0)
            , Waterfall.lineRelative2D (V2 0 0.05)
            , Waterfall.lineRelative2D (V2 (-0.05) 0)
            ]
    in mconcat . take n. iterate (Waterfall.translate2D (V2 0 0.1)) $ oneCrenelation