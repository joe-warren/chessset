{-# LANGUAGE RecordWildCards #-}
module Sets
    ( writeAllSets
    ) where

import qualified Waterfall
import qualified Pawn
import qualified Rook
import qualified Knight
import qualified Bishop
import qualified Queen
import qualified King
import qualified Piece
import qualified Topper
import qualified Skirting
import TextTopper (textTopper)
import Polygonize (polygonize)
import LinearSpaced (linearSpaced)
import Data.Foldable (forM_)
import qualified System.Directory 
import System.FilePath ((</>), (<.>))
import qualified Polygonize as Waterfall
import Piece (PieceData(pieceSolidification))
import Linear (unit, _z)
import Data.Maybe (fromMaybe)
import Split (split)

makeSet :: (Piece.Kind -> Waterfall.Solid) -> FilePath -> IO ()
makeSet f subDir = do
    let directory = "output" </> subDir
    let splitDir = directory </> "split"
    System.Directory.createDirectoryIfMissing True directory
    System.Directory.createDirectoryIfMissing True splitDir
    let pieces = [(kind, f kind) | kind <- Piece.allKinds]
    let res = 0.005
    let write filepath p = do
            putStrLn filepath
            Waterfall.writeSTL res filepath p
    forM_ pieces $ \(kind, p) -> do
          write ( directory </> show kind <.> ".stl") p
          let (a, b) = split p 
          write ( splitDir </> show kind <> "-a" <.> ".stl") a
          write ( splitDir </> show kind <> "-b" <.> ".stl") b
    write (directory </> "All.stl") (linearSpaced 0.1 (snd <$> pieces))

defaultSizes :: (Topper.Args -> Waterfall.Solid) -> Waterfall.Path2D -> (Waterfall.Path2D -> Waterfall.Solid) -> Piece.Kind -> Piece.PieceData
defaultSizes pieceTopper pieceSkirting pieceSolidification kind = 
    Piece.PieceData 
    { Piece.pieceBaseR = Piece.interpolate 1.0 1.6 kind
    , Piece.pieceNeckR = Piece.interpolate 0.25 0.6 kind 
    , Piece.pieceCollarR = Piece.interpolate 0.4 0.8 kind
    , Piece.pieceHeight = Piece.interpolate 3 6.5 kind
    , ..
    }

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
            defaultSizes 
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                (polygonize n)
                kind

indexSidedSet :: Piece.Kind -> Waterfall.Solid
indexSidedSet kind = 
    let sides = 3 + Piece.index kind
        topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.polygonalCrenellations sides 3 0.8)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper sides
                Piece.King -> King.topper
    in  Piece.piece $
            defaultSizes 
                (topper (Piece.interpolate 0.5 0.8 kind))
                (Skirting.crenellatedSkirting . fromMaybe 10 . Piece.pointValue $ kind)
                (polygonize sides)
                kind

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
            defaultSizes
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                Waterfall.revolution
                kind
            
starSet :: Piece.Kind -> Waterfall.Solid
starSet kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper Rook.noCrenellations  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper 6
                Piece.King -> King.topper
    in  Piece.piece $
            defaultSizes
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                (Waterfall.polygonize 3 
                    <> (Waterfall.rotate (unit _z) (pi/3) . Waterfall.polygonize 3))
                kind

                
notationSet :: Waterfall.Font -> Piece.Kind -> Waterfall.Solid
notationSet font kind = 
    let topper = textTopper font . Piece.notation $ kind
    in Piece.piece $
            defaultSizes
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                Waterfall.revolution
                kind

-- | You absolutely can't print this (there are loose letters)
-- It's just included because I'd already built the scaffolding for notation/pointsValue
nameSet :: Waterfall.Font -> Piece.Kind -> Waterfall.Solid
nameSet font kind = 
    let topper = textTopper font . show $ kind
    in Piece.piece $
            defaultSizes
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                Waterfall.revolution
                kind

pointValueSet :: Waterfall.Font -> Piece.Kind -> Waterfall.Solid
pointValueSet font kind = 
    let topper = textTopper font . maybe "∞" show . Piece.pointValue $ kind
    in Piece.piece $
            defaultSizes
                (topper (Piece.interpolate 0.5 0.8 kind))
                Skirting.classicSkirting
                (Waterfall.polygonize 4)
                kind

writeAllSets :: IO ()
writeAllSets = do
    makeSet (nSidedSet 4) "four-sided"
    makeSet (nSidedSet 3) "three-sided"
    makeSet indexSidedSet "variable-sided"
    makeSet roundSet "round"
    makeSet starSet "star"
    monospace <- Waterfall.fontFromSystem "monospace" Waterfall.Bold 12
    sans <- Waterfall.fontFromSystem "sans" Waterfall.Bold 12
    serif <- Waterfall.fontFromSystem "serif" Waterfall.Bold 12
    makeSet (notationSet monospace) "notation"
    makeSet (pointValueSet sans) "pointsValue"
    makeSet (nameSet serif) "name"