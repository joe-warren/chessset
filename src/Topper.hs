module Topper 
( Args (..)
) where
import qualified Waterfall

newtype Args = Args 
    { topperSolidification :: Waterfall.Path2D -> Waterfall.Solid
    }