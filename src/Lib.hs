module Lib
    ( someFunc
    ) where

import qualified Waterfall
import qualified Pawn
import qualified Rook
import qualified Knight
import qualified Bishop
import qualified Queen
import qualified King
import qualified Piece
import Polygonize (polygonize)
import Linear (V2(..), zero, V3 (..))
import Data.Foldable (forM_)

skirting :: Waterfall.Path2D
skirting = Waterfall.pathFrom zero
    [ Waterfall.lineRelative2D (V2 0.05 0.025)
    , Waterfall.lineRelative2D (V2 (-0.05) 0.075)
    , Waterfall.arcViaRelative2D (V2 0.05 0.05) (V2 0 0.1)
    ]

someFunc :: IO ()
someFunc = 
    let rawPieces = 
            [ ("pawn", Pawn.topper 0.1)
            , ("rook", Rook.topper (Rook.polygonalCrenellations 4 3 0.8))  
            , ("knight", Knight.topper)
            , ("bishop", Bishop.topper)
            , ("queen", Queen.topper 4)
            , ("king", King.topper)
            ]
        pieces = 
            [ let interpolate low hi = low + (hi - low) * (fromIntegral i / 5)
              in (name, 
                Waterfall.translate (V3 (interpolate 0 16) 0 0) $ Piece.piece $ Piece.PieceData 
                    { Piece.pieceBaseR = interpolate 1.0 1.6 
                    , Piece.pieceNeckR = interpolate 0.25 0.6
                    , Piece.pieceCollarR = interpolate 0.4 0.8
                    , Piece.pieceHeight = interpolate 3 6.5
                    , Piece.pieceTopper = topper (interpolate 0.5 0.8) 
                    , Piece.pieceSkirting = skirting
                    , Piece.pieceSolidification = polygonize 4 --  Waterfall.revolution -- polygonize (i + 4)
                    }
                )
                | (i, (name, topper)) <- zip [0..] rawPieces
            ]
    in do forM_ pieces $ \(name, p) ->
            Waterfall.writeSTL 0.005 (name <> ".stl") p
          Waterfall.writeSTL 0.005 "all.stl" (foldMap snd pieces)