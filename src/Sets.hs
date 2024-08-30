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
import qualified Profile
import TextTopper (textTopper)
import Polygonize (polygonize)
import LinearSpaced (linearSpaced)
import Data.Foldable (forM_)
import qualified System.Directory 
import System.FilePath ((</>), (<.>))
import qualified Polygonize as Waterfall
import Piece (PieceData(pieceSolidification))
import Linear (unit, _z, V3 (..), V2 (..), zero)
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


simpleProfile :: Double -> Double -> Double -> Waterfall.Path2D -> Double -> Waterfall.Path2D
simpleProfile rBase rNeck rCollar skirting = 
    Profile.makeProfile 
        [ Profile.FixedSizeSegment (Waterfall.line (V2 0 0) (V2 rBase 0))
        , Profile.FixedSizeSegment skirting
        , Profile.VariableSizeSegment (Waterfall.bezier (V2 rBase 0) (V2 (rBase * 0.5) 0) (V2 rNeck 0.5) (V2 rNeck 1))
        , Profile.FixedSizeSegment (Waterfall.line (V2 rNeck 0) (V2 rCollar 0))
        , Profile.FixedSizeSegment skirting
        , Profile.VariableSizeSegment (Waterfall.line (V2 rCollar 0) zero)
        ]

defaultSizes :: (Topper.Args -> Waterfall.Solid) -> Waterfall.Path2D -> (Waterfall.Path2D -> Waterfall.Solid) -> Piece.Kind -> Piece.PieceData
defaultSizes pieceTopper pieceSkirting pieceSolidification kind = 
    let baseR = Piece.interpolate 1.0 1.6 kind
        neckR = Piece.interpolate 0.25 0.6 kind 
        collarR = Piece.interpolate 0.4 0.8 kind
    in Piece.PieceData 
        { Piece.pieceProfile = simpleProfile baseR neckR collarR pieceSkirting
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
    in Piece.piece $
            defaultSizes 
                (topper (Piece.interpolate 0.5 0.9 kind))
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
                (topper (Piece.interpolate 0.5 0.9 kind))
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
                (topper (Piece.interpolate 0.5 0.9 kind))
                Skirting.classicSkirting
                Waterfall.revolution
                kind

                
tallSet :: Piece.Kind -> Waterfall.Solid
tallSet kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.radialCrenellations 7)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper 7
                Piece.King -> King.topper
        baseR = Piece.interpolate 1.0 1.6 kind
        neckR = Piece.interpolate 0.25 0.6 kind 
        collarR = Piece.interpolate 0.8 1.4 kind
    in  Piece.piece $
            Piece.PieceData 
                { Piece.pieceProfile = simpleProfile baseR neckR collarR Skirting.classicSkirting
                , Piece.pieceHeight = Piece.interpolate 6 12 kind
                , Piece.pieceTopper = topper (Piece.interpolate 0.9 1.5 kind)
                , Piece.pieceSolidification = Waterfall.revolution
                }

shortKingSet :: Piece.Kind -> Waterfall.Solid
shortKingSet kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.radialCrenellations 7)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper 7
                Piece.King -> King.topper
        baseR = 
            case kind of
                Piece.King -> 1.2
                _ -> Piece.interpolate 1.0 1.6 kind
        neckR = Piece.interpolate 0.25 0.6 kind 
        collarR = Piece.interpolate 0.4 0.8 kind
    in  Piece.piece $
            Piece.PieceData 
                { Piece.pieceHeight = 
                    case kind of 
                        Piece.King -> 3
                        _ -> Piece.interpolate 3 6.5 kind
                , Piece.pieceProfile = simpleProfile baseR neckR collarR Skirting.classicSkirting
                , Piece.pieceTopper = topper (Piece.interpolate 0.5 0.9 kind)
                , Piece.pieceSolidification = Waterfall.revolution
                }

alternateProfile :: Double -> Double -> Waterfall.Path2D
alternateProfile r = 
    let segment :: Double -> Double -> Double -> Double -> Waterfall.Path2D
        segment height rLow rHi curviness = 
             Waterfall.bezier (V2 rLow 0) (V2 (rLow + curviness) 0) (V2 (rHi + curviness) height) (V2 rHi height)
    in Profile.makeProfile 
        [ Profile.FixedSizeSegment (Waterfall.line (V2 0 0) (V2 r 0))
        , Profile.FixedSizeSegment (segment 0.4 r r (r * 0.15))
        , Profile.FixedSizeSegment (Waterfall.line (V2 r 0 ) (V2 (r*0.2) 0))
        , Profile.VariableSizeSegment (segment 0.2 (r*0.1) (r * 0.2) (r * 0.3))
        , Profile.VariableSizeSegment (segment 0.25 (r*0.2) (r * 0.3) (r * 0.3))
        , Profile.VariableSizeSegment (segment 0.3 (r*0.3) (r * 0.4) (r * 0.3))
        , Profile.FixedSizeSegment (Waterfall.line (V2 (r*0.4) 0) (V2 0 0))
        ]

alternateProfileSet :: Piece.Kind -> Waterfall.Solid
alternateProfileSet kind = 
    let topper = 
            case kind of 
                Piece.Pawn -> Pawn.topper 0.1
                Piece.Rook -> Rook.topper (Rook.radialCrenellations 7)  
                Piece.Knight -> Knight.topper
                Piece.Bishop -> Bishop.topper 
                Piece.Queen -> Queen.topper 7
                Piece.King -> King.topper
        baseR = Piece.interpolate 1.0 1.6 kind
    in  Piece.piece $
            Piece.PieceData 
                { Piece.pieceHeight = Piece.interpolate 3 6.5 kind
                , Piece.pieceProfile = alternateProfile baseR
                , Piece.pieceTopper = topper (baseR*0.6)
                , Piece.pieceSolidification = Waterfall.revolution
                }
            
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
                (topper (Piece.interpolate 0.5 0.9 kind))
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
                (topper (Piece.interpolate 0.5 0.9 kind))
                Skirting.classicSkirting
                (Waterfall.polygonize 4)
                kind

writeAllSets :: IO ()
writeAllSets = do
    makeSet alternateProfileSet "alternate-profile"
    makeSet roundSet "round"
    makeSet tallSet "tall"
    makeSet shortKingSet "short-king"
    makeSet (nSidedSet 4) "four-sided"
    makeSet (nSidedSet 3) "three-sided"
    makeSet indexSidedSet "variable-sided"
    makeSet starSet "star"
    monospace <- Waterfall.fontFromSystem "monospace" Waterfall.Bold 12
    sans <- Waterfall.fontFromSystem "sans" Waterfall.Bold 12
    serif <- Waterfall.fontFromSystem "serif" Waterfall.Bold 12
    makeSet (notationSet monospace) "notation"
    makeSet (pointValueSet sans) "pointsValue"
    makeSet (nameSet serif) "name"

    Waterfall.writeSTL 0.05 ("output" </> "spacer" <.> ".stl") (Waterfall.scale (V3 0.3 0.3 1) Waterfall.unitCube)