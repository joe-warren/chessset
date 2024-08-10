{-# LANGUAGE RecordWildCards #-}
module Piece 
( PieceData (..)
, piece
) where

import qualified Waterfall
import Linear (V3 (..), _z)
import Control.Lens ((^.))
import qualified Base

data PieceData  = PieceData 
    { pieceBaseR :: Double
    , pieceNeckR :: Double
    , pieceCollarR :: Double
    , pieceHeight :: Double
    , pieceTopper :: Waterfall.Solid
    , pieceSkirting :: Waterfall.Path2D
    }

piece :: PieceData -> Waterfall.Solid
piece PieceData {..} = 
    let topH =  maybe 0 ((^. _z). snd) . Waterfall.axisAlignedBoundingBox $ pieceTopper
        baseH = pieceHeight - topH
        baseProfile = Base.profile pieceBaseR pieceNeckR pieceCollarR baseH pieceSkirting
        base = Waterfall.revolution baseProfile
    in Waterfall.translate (V3 0 0 baseH) pieceTopper <> base