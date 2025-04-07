{-# LANGUAGE TupleSections #-}
module Boards 
( renderBoard
, renderAllBoards
) where

import qualified Waterfall
import qualified Waterfall.SVG
import qualified Piece
import qualified Sets
import Control.Lens ((^.))
import Linear
import Data.Function ((&))
import System.FilePath ((</>))
import Data.Maybe (catMaybes)

data Colour = White | Black

combineSets :: (Piece.Kind -> Waterfall.Solid) -> (Piece.Kind -> Waterfall.Solid) -> Colour -> Piece.Kind -> Waterfall.Solid
combineSets w _ White = w
combineSets _ b Black = b

renderBoard :: ((Int, Int) -> Maybe (Colour, Piece.Kind)) -> (Colour -> Piece.Kind -> Waterfall.Solid) -> FilePath -> IO ()
renderBoard boardLayout sets path = 
    let squareWidth = 4
        pieces = mconcat . catMaybes $
            [   case boardLayout (x, y) of
                    Nothing -> Nothing
                    Just (col, kind) -> Just . Waterfall.translate (V3 (fromIntegral x * squareWidth) (fromIntegral y * squareWidth) 0) $ sets col kind 
                | x <- [1..8]
                , y <- [1..8]
            ]
        borderWidth = 1 
        baseWidth = squareWidth * 8 + borderWidth * 2
        baseHeight = 0.5
        boardBase =
            Waterfall.centeredCube 
                & Waterfall.translate (-0.5 *^ unit _z)
                & Waterfall.scale (V3 baseWidth baseWidth baseHeight)
                & Waterfall.translate (V3 (squareWidth * 4.5) (squareWidth *4.5) 0)
                & Waterfall.roundConditionalFillet (\(a,b) -> if (a ^. _xy == b ^. _xy) then Just 0.3 else Nothing)

        squareHeight = 0.1
        boardSquares = mconcat . catMaybes $
            [ if ((x + y) `mod` 2) == 0
                    then Nothing
                    else Just . Waterfall.translate (V3 (fromIntegral x * squareWidth) (fromIntegral y * squareWidth) 0)
                             . Waterfall.scale (V3 (squareWidth * 0.9) (squareWidth * 0.9) (squareHeight * 2)) 
                             $ Waterfall.centeredCube
            | x <- [1..8] :: [Int]
            , y <- [1..8] :: [Int]
            ]
        board = pieces <> Waterfall.translate (-0.01  *^ unit _z) (boardBase `Waterfall.difference` boardSquares)

        diagram = 
            Waterfall.uScale2D 40 $ 
                Waterfall.rotate2D (pi/2 + pi/6) $
                Waterfall.solidDiagram (V3 (1) (-1) 1) board
    in Waterfall.SVG.writeDiagramSVG ("output" </> path) diagram 

homeRow :: Int -> Maybe Piece.Kind
homeRow 1 = Just Piece.Rook
homeRow 2 = Just Piece.Knight
homeRow 3 = Just Piece.Bishop
homeRow 5 = Just Piece.Queen
homeRow 4 = Just Piece.King
homeRow 6 = Just Piece.Bishop
homeRow 7 = Just Piece.Knight 
homeRow 8 = Just Piece.Rook
homeRow _ = Nothing

startingLayout :: (Int, Int) -> Maybe (Colour, Piece.Kind)
startingLayout (x, 1) = (White,) <$> homeRow x
startingLayout (_, 2) = Just (White, Piece.Pawn) 
startingLayout (_, 7) = Just (Black, Piece.Pawn) 
startingLayout (x, 8) = (Black,) <$> homeRow x
startingLayout _ = Nothing

renderAllBoards :: IO ()
renderAllBoards = renderBoard (startingLayout) (combineSets Sets.roundSet (Sets.nSidedSet 4)) "board.svg"