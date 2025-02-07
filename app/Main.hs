{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Char (digitToInt, isDigit)
import Data.List.Split (splitOn)



data Color' = White | Black deriving (Enum, Eq)
data PieceType = Rook | Knight | Bishop | Queen | King | Pawn deriving (Enum, Eq)
data ColorType = SquareBG | SquareEmp | PieceFG deriving (Enum, Eq)
type Piece = (PieceType, Color')


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

getColor :: (Ord b, Floating b) => ColorType -> Color' -> Colour b
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


pieceDg :: PieceType -> Color' -> Diagram B
pieceDg name color = text [pieceChar name] # fc (pieceColor color) # font "freesans"

sqColor :: (Ord a, Floating a) => Color' -> Colour a
sqColor = getColor SquareBG

sqColorEmp :: (Ord a, Floating a) => Color' -> Colour a
sqColorEmp = getColor SquareEmp

pieceColor :: (Ord a, Floating a) => Color' -> Colour a
pieceColor = getColor PieceFG

squareDg :: Color' -> Maybe Piece -> Diagram B
squareDg color maybePiece = let sq = square 1 # fc (sqColor color) # lw none in
        case maybePiece of
                Just piece -> uncurry pieceDg piece `atop` sq
                Nothing -> sq

rankDg :: Bool -> Int -> String -> Diagram B
rankDg isBlackTurn number extFen = do
        let start = mod number 2
        let colors = map (\x -> if odd x then Black else White) [start..(start+8)]
        let pieces = map fenToPiece extFen
        let squareData = zip colors pieces
        let squares = map (uncurry squareDg) squareData
        let turnedSquares = if isBlackTurn then map (# rotateBy (1/2)) squares else squares
        foldl1 (|||) turnedSquares

fenToBoard :: String -> Diagram B
fenToBoard fen = do
        let [ranks, turn, castling, ep, hc, fc] = words fen
        let extRanks = concatMap (\char ->
                if isDigit char
                        then replicate (digitToInt char) '*'
                        else [char]
                ) ranks
        let rankData = zip (reverse [1..8]) (splitOn "/" extRanks)
        let isBlackTurn = turn == "b"
        let ranksDg = map (uncurry (rankDg isBlackTurn)) rankData
        let board = foldl1 (===) ranksDg
        if isBlackTurn then board # rotateBy (1/2) else board

main :: IO ()
main = mainWith $ fenToBoard "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

