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
import LinearSpaced (linearSpaced)
import Linear (V2(..), zero)
import Data.Foldable (forM_)
import qualified System.Directory 
import System.FilePath ((</>), (<.>))


skirting :: Waterfall.Path2D
skirting = Waterfall.pathFrom zero
    [ Waterfall.lineRelative2D (V2 0.05 0.025)
    , Waterfall.lineRelative2D (V2 (-0.05) 0.075)
    , Waterfall.arcViaRelative2D (V2 0.05 0.05) (V2 0 0.1)
    ]

makeSet :: (Piece.Kind -> Waterfall.Solid) -> FilePath -> IO ()
makeSet f directory = do
    System.Directory.createDirectoryIfMissing True directory
    let pieces = [(kind, f kind) | kind <- Piece.allKinds]
    let res = 0.005
    forM_ pieces $ \(kind, p) ->
          Waterfall.writeSTL res ( directory </> show kind <.> ".stl") p
    Waterfall.writeSTL res (directory </> "All.stl") (linearSpaced 0.1 (snd <$> pieces))

nSidedSet :: Int -> Piece.Kind -> Waterfall.Solid
nSidedSet n kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.polygonalCrenellations n 3 0.8)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper n
                Piece.King -> King.topper
    in  Piece.piece $
            Piece.PieceData 
            { Piece.pieceBaseR = Piece.interpolate 1.0 1.6 kind
            , Piece.pieceNeckR = Piece.interpolate 0.25 0.6 kind 
            , Piece.pieceCollarR = Piece.interpolate 0.4 0.8 kind
            , Piece.pieceHeight = Piece.interpolate 3 6.5 kind
            , Piece.pieceTopper = topper (Piece.interpolate 0.5 0.8 kind) 
            , Piece.pieceSkirting = skirting
            , Piece.pieceSolidification = polygonize n --  Waterfall.revolution -- polygonize (i + 4)
            }

roundSet :: Piece.Kind -> Waterfall.Solid
roundSet kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.radialCrenellations 7)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper 7
                Piece.King -> King.topper
    in  Piece.piece $
            Piece.PieceData 
            { Piece.pieceBaseR = Piece.interpolate 1.0 1.6 kind
            , Piece.pieceNeckR = Piece.interpolate 0.25 0.6 kind 
            , Piece.pieceCollarR = Piece.interpolate 0.4 0.8 kind
            , Piece.pieceHeight = Piece.interpolate 3 6.5 kind
            , Piece.pieceTopper = topper (Piece.interpolate 0.5 0.8 kind) 
            , Piece.pieceSkirting = skirting
            , Piece.pieceSolidification = Waterfall.revolution
            }

someFunc :: IO ()
someFunc = do
    makeSet (nSidedSet 4) "four-sided"
    makeSet (nSidedSet 3) "three-sided"
    makeSet roundSet "round"