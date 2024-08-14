{-# LANGUAGE RecordWildCards #-}
module Piece 
( PieceData (..)
, piece
) where

import qualified Waterfall
import Linear (V3 (..), _z)
import Control.Lens ((^.))
import qualified Base
import qualified Topper

data PieceData  = PieceData 
    { pieceBaseR :: Double
    , pieceNeckR :: Double
    , pieceCollarR :: Double
    , pieceHeight :: Double
    , pieceTopper :: Topper.Args -> Waterfall.Solid
    , pieceSkirting :: Waterfall.Path2D
    , pieceSolidification :: Waterfall.Path2D -> Waterfall.Solid
    }

piece :: PieceData -> Waterfall.Solid
piece PieceData {..} = 
    let topperSolidification = pieceSolidification
        topper = pieceTopper $ Topper.Args {..}
        topH =  maybe 0 ((^. _z). snd) . Waterfall.axisAlignedBoundingBox $ topper 
        baseH = pieceHeight - topH
        baseProfile = Base.profile pieceBaseR pieceNeckR pieceCollarR baseH pieceSkirting
        base = pieceSolidification baseProfile
    in Waterfall.translate (V3 0 0 baseH) topper <> base