module Lib
    ( someFunc
    ) where

import qualified Waterfall
import qualified Base
import qualified Pawn
import qualified Piece
import Linear (V2(..), zero)

skirting :: Waterfall.Path2D
skirting = Waterfall.pathFrom zero
    [ Waterfall.lineRelative2D (V2 0.05 0.025)
    , Waterfall.lineRelative2D (V2 (-0.05) 0.075)
    , Waterfall.arcViaRelative2D (V2 0.05 0.05) (V2 0 0.1)
    ]

someFunc :: IO ()
someFunc = 
    let pawn = Piece.piece $ Piece.PieceData 
            { Piece.pieceBaseR = 1.0
            , Piece.pieceNeckR = 0.25
            , Piece.pieceCollarR = 0.4
            , Piece.pieceHeight = 3
            , Piece.pieceTopper = Pawn.topper 0.5 0.1
            , Piece.pieceSkirting = skirting
            }
    in Waterfall.writeSTL 0.005 "pawn.stl" $ pawn
