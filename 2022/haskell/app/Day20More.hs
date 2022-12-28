module Main where

import Control.Lens
import ClassyPrelude hiding (maximumBy)
import Data.Coerce (coerce)
import Data.Foldable (maximumBy, Foldable)
import Data.List ((!!), nub)
import Data.Ord (comparing)
import Data.Vector ((!), (!?), generate)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

type TileId = Int

data Tile a = Tile TileId a
  deriving (Eq, Functor, Show)

data Pixel = PixDot | PixHash
  deriving (Eq, Show)

newtype RawTile p = RawTile (Vector (Vector p))
  deriving (Eq, Show)

type RightVal = Int
type DownVal = Int


type Board = Vector (Vector (Maybe BoardPiece))


parseTileId :: Parser TileId
parseTileId = do
  void $ string "Tile "
  i <- decimal
  void $ string ":"
  pure i

parsePixel :: Parser Pixel
parsePixel =
  (char '.' $> PixDot) <|>
  (char '#' $> PixHash)

allPixelsToRawTile :: [[p]] -> RawTile p
allPixelsToRawTile = RawTile . fromList . fmap fromList

parseRawTile :: Parser (RawTile Pixel)
parseRawTile = do
  x <- count 10 (count 10 parsePixel <* newline)
  pure $ allPixelsToRawTile x

parseTile :: Parser (Tile (RawTile Pixel))
parseTile = do
  tileId <- parseTileId
  void newline
  rawTile <- parseRawTile
  pure $ Tile tileId rawTile

emptyBoard :: Int -> Board
emptyBoard sideLength = replicate sideLength (replicate sideLength Nothing)

parseTiles :: Parser [Tile (RawTile Pixel)]
parseTiles = sepBy parseTile newline

lookupBoard :: Int -> Int -> Board -> Maybe BoardPiece
lookupBoard row col board = do
  r <- board !? row
  maybePiece <- r !? col
  maybePiece

setPieceInBoard :: Int -> Int -> BoardPiece -> Board -> Board
setPieceInBoard row col nextPiece board =
  set (ix row . ix col . traverse) nextPiece board

type UnusedPieces = Set TileId

type BoardPiece = (TileId, RightVal, DownVal)

initUnusedPieces :: [Tile (RawTile Pixel)] -> UnusedPieces
initUnusedPieces = undefined

removePiece :: TileId -> UnusedPieces -> UnusedPieces
removePiece tileId unusedPieces = undefined

allPiecesAllRotations :: UnusedPieces -> [(UnusedPieces, BoardPiece)]
allPiecesAllRotations = undefined

pieceWithSide :: Int -> UnusedPieces -> [(TileId, Int, Int, Int)]
pieceWithSide i unusedPieces = 
  for unusedPieces \(tileId, piece) ->
    for piece \(sideDirection, sideVal) ->
      if sideVal == i
        then 
          case sideDirection of
            Top ->
              (tileId, rightSide piece, flipI $ bottomSide piece, flipI $ leftSide piece)
            Rght ->
              (tileId, flipI $ bottomSide piece, flipI $ leftSide piece, topSide piece)
  -- for piece in unusedPieces:
  --   for side in peice:
  --     if side == i then (peice tileid, nextside, nextnextside, nextnextnextside)
  --     else ()
  --   for side in flip peice:
      
  -- filterUnusedPieces unusedPieces

piecesWithTop :: Int -> UnusedPieces -> [(UnusedPieces, BoardPiece)]
piecesWithTop downVal unusedPieces = do
  (tileId, thisPieceRightVal, thisPieceDownVal, _) <- pieceWithSide downVal unusedPieces
  pure (removePiece tileId unusedPieces, (tileId, thisPieceRightVal, thisPieceDownVal))

piecesWithLeft :: Int -> UnusedPieces -> [(UnusedPieces, BoardPiece)]
piecesWithLeft rightVal = undefined

piecesWithTopAndLeft :: Int -> Int -> UnusedPieces -> [(UnusedPieces, BoardPiece)]
piecesWithTopAndLeft downVal rightVal = undefined

findPieceFor
  :: UnusedPieces -> Maybe BoardPiece -> Maybe BoardPiece -> [(UnusedPieces, BoardPiece)]
findPieceFor unusedPieces maybeAbovePiece maybePrevPiece =
  case (maybeAbovePiece, maybePrevPiece) of
    -- upper left corner of the board
    (Nothing, Nothing) -> allPiecesAllRotations unusedPieces
    (Just (_, _, downVal), Nothing) -> piecesWithTop downVal unusedPieces
    (Nothing, Just (_, rightVal, _)) -> piecesWithLeft rightVal unusedPieces
    (Just (_, _, downVal), Just (_, rightVal, _)) ->
      piecesWithTopAndLeft downVal rightVal unusedPieces

fillNextPieceBoard :: UnusedPieces ->  Int -> Int -> Board -> [(UnusedPieces, Board)]
fillNextPieceBoard unusedPieces row col board = do
  let maybeAbovePiece = lookupBoard (row - 1) col board
      maybePrevPiece = lookupBoard row (col - 1) board
  (newUnusedPieces, nextPiece) <- findPieceFor unusedPieces maybeAbovePiece maybePrevPiece
  pure $ (newUnusedPieces, setPieceInBoard row col nextPiece board)

solvePuzzle :: [Tile (RawTile Pixel)] -> Maybe Board
solvePuzzle inputPieces =
  let sideLength = floor $ sqrt (fromIntegral (length inputPieces))
      board = emptyBoard sideLength
      unusedPieces = initUnusedPieces inputPieces
  in
  traceShow sideLength $
  traceShow board $
  go sideLength unusedPieces 0 0 board
  where
  go :: Int -> UnusedPieces -> Int -> Int -> Board -> [Board]
  go sideLength unusedPieces row col board
    | row == sideLength = pure board
    | col == sideLength = go unusedPieces sideLength (row + 1) 0 board
    | otherwise = do
        (newUnusedPieces, newBoard) <- fillNextPieceBoard unusedPieces row col board
        go sideLength newUnusedPieces row (col + 1) newBoard

main :: IO ()
main = do
  let inputFile = "input-day20-example2"
  -- let inputFile = "input-day20-example"
  -- let inputFile = "input-day20"
  input <- readFile inputFile
  let eitherParseRes = parse parseTiles inputFile $ decodeUtf8 input
  case eitherParseRes of
    Left err -> print err
    Right tiles -> do
      let !solution = solvePuzzle tiles
      pure ()
  pure ()
