module Main where

import Data.List ((!!))
import ClassyPrelude hiding (maximumBy)
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

newtype TileId = TileId { unTileId :: Int }
  deriving (Eq, Show)

data Tile a = Tile TileId a
  deriving (Eq, Functor, Show)

data Pixel = PixDot | PixHash
  deriving (Eq, Show)

newtype RawTile = RawTile (Vector (Vector Pixel))
  deriving (Eq, Show)

newtype SideVal = SideVal Int
  deriving (Eq, Show)

data RawTileWithInfo = RawTileWithInfo RawTile [SideVal]

-- |
-- >>> pixelVecToInt [PixHash, PixDot, PixHash, PixDot]
-- 10
-- >>> pixelVecToInt [PixHash]
-- 1
-- >>> pixelVecToInt []
-- 0
-- >>> pixelVecToInt [PixDot, PixDot]
-- 0
-- >>> pixelVecToInt [PixHash, PixHash]
-- 3
pixelVecToInt :: (Element (f Pixel) ~ Pixel, MonoFoldable (f Pixel), Foldable f) => f Pixel -> Int
pixelVecToInt = foldl' go 0
  where
  go :: Int -> Pixel -> Int
  go i PixDot = 2 * i
  go i PixHash = 1 + 2 * i

rawTileGetTopLeftToRight :: Vector (Vector Pixel) -> SideVal
rawTileGetTopLeftToRight = SideVal . pixelVecToInt . headEx

rawTileGetTopFlipped :: Vector (Vector Pixel) -> SideVal
rawTileGetTopFlipped = SideVal . pixelVecToInt . reverse . headEx

rawTileGetLeftTopToBottom :: Vector (Vector Pixel) -> SideVal
rawTileGetLeftTopToBottom = SideVal . pixelVecToInt . fmap headEx

rawTileGetLeftFlipped :: Vector (Vector Pixel) -> SideVal
rawTileGetLeftFlipped = SideVal . pixelVecToInt . reverse . fmap headEx

rawTileGetRightTopToBottom :: Vector (Vector Pixel) -> SideVal
rawTileGetRightTopToBottom = SideVal . pixelVecToInt . fmap lastEx

rawTileGetRightFlipped :: Vector (Vector Pixel) -> SideVal
rawTileGetRightFlipped = SideVal . pixelVecToInt . reverse . fmap lastEx

rawTileGetBottomLeftToRight :: Vector (Vector Pixel) -> SideVal
rawTileGetBottomLeftToRight = SideVal . pixelVecToInt . lastEx

rawTileGetBottomFlipped :: Vector (Vector Pixel) -> SideVal
rawTileGetBottomFlipped = SideVal . pixelVecToInt . reverse . lastEx

-- |
-- >>> rawTileToSideVals $ RawTile $ fromList $ fmap fromList [[PixDot,PixDot,PixHash,PixHash,PixDot,PixHash,PixDot,PixDot,PixHash,PixDot],[PixHash,PixHash,PixDot,PixDot,PixHash,PixDot,PixDot,PixDot,PixDot,PixDot],[PixHash,PixDot,PixDot,PixDot,PixHash,PixHash,PixDot,PixDot,PixHash,PixDot],[PixHash,PixHash,PixHash,PixHash,PixDot,PixHash,PixDot,PixDot,PixDot,PixHash],[PixHash,PixHash,PixDot,PixHash,PixHash,PixDot,PixHash,PixHash,PixHash,PixDot],[PixHash,PixHash,PixDot,PixDot,PixDot,PixHash,PixDot,PixHash,PixHash,PixHash],[PixDot,PixHash,PixDot,PixHash,PixDot,PixHash,PixDot,PixDot,PixHash,PixHash],[PixDot,PixDot,PixHash,PixDot,PixDot,PixDot,PixDot,PixHash,PixDot,PixDot],[PixHash,PixHash,PixHash,PixDot,PixDot,PixDot,PixHash,PixDot,PixHash,PixDot],[PixDot,PixDot,PixHash,PixHash,PixHash,PixDot,PixDot,PixHash,PixHash,PixHash]]
-- [SideVal 210,SideVal 300,SideVal 498,SideVal 318,SideVal 89,SideVal 616,SideVal 231,SideVal 924]
rawTileToSideVals :: RawTile -> [SideVal]
rawTileToSideVals (RawTile vecVec) =
  [ rawTileGetTopLeftToRight vecVec
  , rawTileGetTopFlipped vecVec
  , rawTileGetLeftTopToBottom vecVec
  , rawTileGetLeftFlipped vecVec
  , rawTileGetRightTopToBottom vecVec
  , rawTileGetRightFlipped vecVec
  , rawTileGetBottomLeftToRight vecVec
  , rawTileGetBottomFlipped vecVec
  ]

parseTileId :: Parser TileId
parseTileId = do
  void $ string "Tile "
  i <- decimal
  void $ string ":"
  pure $ TileId i

parsePixel :: Parser Pixel
parsePixel =
  (char '.' $> PixDot) <|>
  (char '#' $> PixHash)

allPixelsToRawTile :: [[Pixel]] -> RawTile
allPixelsToRawTile = RawTile . fromList . fmap fromList

parseRawTile :: Parser RawTile
parseRawTile = do
  x <- count 10 (count 10 parsePixel <* newline)
  pure $ allPixelsToRawTile x

parseTile :: Parser (Tile RawTile)
parseTile = do
  tileId <- parseTileId
  void newline
  rawTile <- parseRawTile
  pure $ Tile tileId rawTile

parseTiles :: Parser [Tile RawTile]
parseTiles = sepBy parseTile newline

rawTileGetInfo :: RawTile -> RawTileWithInfo
rawTileGetInfo rawTile = RawTileWithInfo rawTile $ rawTileToSideVals rawTile

solvePuzzle :: [Tile RawTile] -> [[Tile RawTile]]
solvePuzzle inputTiles@(Tile id r@(RawTile vecVec) : ts) =
  let inputTilesWithSideVals = fmap (fmap rawTileGetInfo) inputTiles :: [Tile RawTileWithInfo]
      howManyOnSide = sqrt $ fromIntegral $ length inputTiles :: Float
  in
  traceShow howManyOnSide $ traceShow r $ traceShow (rawTileToSideVals r) undefined

main :: IO ()
main = do
  -- let inputFile = "input-day20-example"
  let inputFile = "input-day20"
  input <- readFile inputFile
  let eitherParseRes = parse parseTiles inputFile $ decodeUtf8 input
  case eitherParseRes of
    Left err -> print err
    Right tiles -> do
      let !solution = solvePuzzle tiles
      pure ()
  pure ()
