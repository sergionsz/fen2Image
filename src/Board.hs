{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}

module Board (fenToimage) where

import Diagrams.Prelude hiding (width, apply, Color)
import Data.Char (digitToInt, isDigit)
import Data.List.Split (splitOn)
import Diagrams.Backend.Cairo

data Color = White | Black deriving (Enum, Eq)
data PieceType = Rook | Knight | Bishop | Queen | King | Pawn deriving (Enum, Eq)
data ColorType = SquareBG | SquareEmp | PieceFG deriving (Enum, Eq)
type Piece = (PieceType, Color)

textSize :: Num a => a
textSize = 20

apply :: [b -> c] -> [b] -> [c]
apply = zipWith ($)

fenToPiece :: Char -> Maybe Piece
fenToPiece char = case char of
  'r' -> Just (Rook, Black)
  'n' -> Just (Knight, Black)
  'b' -> Just (Bishop, Black)
  'q' -> Just (Queen, Black)
  'k' -> Just (King, Black)
  'p' -> Just (Pawn, Black)
  'R' -> Just (Rook, White)
  'N' -> Just (Knight, White)
  'B' -> Just (Bishop, White)
  'Q' -> Just (Queen, White)
  'K' -> Just (King, White)
  'P' -> Just (Pawn, White)
  _ -> Nothing

getColor :: (Ord b, Floating b) => ColorType -> Color -> Colour b
getColor colorType side = sRGB24read $ case (colorType, side) of
  (SquareBG, Black) -> "2d7d6e"
  (SquareBG, White) -> "8f9795"
  (PieceFG, Black) -> "312727"
  (PieceFG, White) -> "f7f7f7"
  (SquareEmp, Black) -> "5f8848"
  (SquareEmp, White) -> "a69b62"

pieceChar :: PieceType -> Char
pieceChar name = case name of
  Rook -> '♜'
  Knight -> '♞'
  Bishop -> '♝'
  Queen -> '♛'
  King -> '♚'
  Pawn -> '♟'


pieceDg :: PieceType -> Color -> Diagram B
pieceDg name color = text [pieceChar name] # fc (pieceColor color)

sqColor :: (Ord a, Floating a) => Color -> Colour a
sqColor = getColor SquareBG

sqColorEmp :: (Ord a, Floating a) => Color -> Colour a
sqColorEmp = getColor SquareEmp

pieceColor :: (Ord a, Floating a) => Color -> Colour a
pieceColor = getColor PieceFG

squareDg :: Color -> Maybe Piece -> Bool -> Diagram B
squareDg color maybePiece highlighted = do
  let sq = square 1 # fc ((if highlighted then sqColorEmp else sqColor) color) # lw none
  case maybePiece of
    Just piece -> uncurry pieceDg piece `atop` sq
    Nothing -> sq

rankDg :: Bool -> Int -> String -> [Bool] -> Diagram B
rankDg isBlackTurn number extFen highlightedRank = do
  let rangeStart = mod number 2
  let colors = map (\x -> if odd x then Black else White) [rangeStart..(rangeStart+8)]
  let pieces = map fenToPiece extFen
  let squares = apply (apply (map squareDg colors) pieces) highlightedRank
  let turnedSquares = if isBlackTurn then map (# rotateBy (1/2)) squares else squares
  foldl1 (|||) turnedSquares

rankCoords :: Bool -> Diagram B
rankCoords isBlackTurn = do
  let numbers = ['1'..'8']
  let numbersDg = map ((# fontSize textSize) . text . (:[])) (if isBlackTurn then numbers else reverse numbers)
  let sqs = map (\x -> x `atop` (square 1 # lw none)) numbersDg
  foldl1 (===) sqs

fileCoords :: Bool -> Diagram B
fileCoords isBlackTurn = do
  let files = ['a'..'h']
  let filesDg = map ((# fontSize textSize) . text . (:[])) (" " ++ (if isBlackTurn then reverse files else files))
  let sqs = map (\x -> x `atop` (square 1 # lw none)) filesDg
  foldl1 (|||) sqs

getHighlightedBoard :: Maybe (String, String) -> [[Bool]]
getHighlightedBoard lastMove = case lastMove of
    Just ((fromFile: fromRank: _), (toFile: toRank: _)) -> do
      let isHighlighted file rank = (file == fromFile && rank == fromRank) || (file == toFile && rank == toRank)
      reverse [[isHighlighted file rank | file <- ['a'..'h']] | rank <- ['1'..'8']]
    _ -> replicate 8 (replicate 8 False)

fenToimage :: String -> Maybe (String, String) -> Maybe Double -> FilePath -> IO ()
fenToimage fen maybeLastMove maybeWidth = do
  let highlightBoard = getHighlightedBoard maybeLastMove
  case words fen of
    (ranks:currentTurn:_) -> do
      let f = (\char -> if isDigit char
          then replicate (digitToInt char) '*'
          else [char])
      let extRanks = splitOn "/" (concatMap f ranks)
      let rankNumbers = reverse [1..8]
      let isBlackTurn = currentTurn == "b"
      -- TODO: Find a better way than apply (apply)
      let ranksDg = apply (apply (map (rankDg isBlackTurn) rankNumbers) extRanks) highlightBoard
      let board = foldl1 (===) ranksDg
      let turnedBoard = centerXY (if isBlackTurn then board # rotateBy (1/2) else board)
      let rCoords = centerXY $ rankCoords isBlackTurn
      let fCoords = fileCoords isBlackTurn
      let boardWithCoords = (fCoords === (rCoords ||| turnedBoard ||| rCoords) === fCoords) # bg white
      let width = case maybeWidth of {
        Just number -> number ;
        _ -> 800
      }
      let dimensions = mkSizeSpec2D (Just width) Nothing
      let custonRender diagram file = renderCairo file dimensions diagram
      custonRender boardWithCoords
    _ -> error "Given FEN is wrong"
