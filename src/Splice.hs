module Splice (
    splice2D
) where

import qualified Waterfall
import Linear (V2)
splice2D :: Waterfall.Path2D -> V2 Double -> (V2 Double, Waterfall.Path2D)
splice2D path pnt = 
    let (s, e) = Waterfall.pathEndpoints path
      in (pnt + e - s , Waterfall.translate2D (pnt - s) path)