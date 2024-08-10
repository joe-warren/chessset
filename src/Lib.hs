module Lib
    ( someFunc
    ) where

import qualified Waterfall
import qualified Base
import Linear (V2(..), zero)


skirting :: Waterfall.Path2D
skirting = Waterfall.pathFrom zero
    [ Waterfall.lineRelative2D (V2 0.05 0.025)
    , Waterfall.lineRelative2D (V2 (-0.05) 0.075)
    , Waterfall.arcViaRelative2D (V2 0.05 0.05) (V2 0 0.1)
    ]

someFunc :: IO ()
someFunc = Waterfall.writeSTL 0.005 "base.stl" . Waterfall.revolution $ Base.profile 1.0 0.2 0.4 2.4 skirting
