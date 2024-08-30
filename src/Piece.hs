{-# LANGUAGE RecordWildCards #-}
module Piece 
( PieceData (..)
, Kind (..)
, piece
, index
, interpolate
, pointValue
, notation
, allKinds
) where

import qualified Waterfall
import Linear (V3 (..), _z)
import Control.Lens ((^.))
import qualified Topper

data Kind = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Ord, Enum, Bounded, Show)

allKinds :: [Kind]
allKinds = [minBound..maxBound]

index :: Kind -> Int
index = fromEnum

interpolate :: Floating f => f -> f -> Kind -> f 
interpolate low hi kind = low + (hi - low) * (fromIntegral (index kind) / fromIntegral (index maxBound))

pointValue :: Kind -> Maybe Int
pointValue Pawn = Just 1
pointValue Knight = Just 3
pointValue Bishop = Just 3
pointValue Rook = Just 5
pointValue Queen = Just 9
pointValue King = Nothing

notation :: Kind -> String
notation Pawn = "P"
notation Knight = "N"
notation Bishop = "B"
notation Rook = "R"
notation Queen = "Q"
notation King = "K"

data PieceData  = PieceData 
    { pieceProfile :: Double -> Waterfall.Path2D
    , pieceHeight :: Double
    , pieceTopper :: Topper.Args -> Waterfall.Solid
    , pieceSolidification :: Waterfall.Path2D -> Waterfall.Solid
    }

piece :: PieceData -> Waterfall.Solid
piece PieceData {..} = 
    let topperSolidification = pieceSolidification
        topper = pieceTopper $ Topper.Args {..}
        topH =  maybe 0 ((^. _z). snd) . Waterfall.axisAlignedBoundingBox $ topper 
        baseH = pieceHeight - topH
        baseProfile = pieceProfile baseH
        base = pieceSolidification baseProfile
    in Waterfall.translate (V3 0 0 baseH) topper <> base