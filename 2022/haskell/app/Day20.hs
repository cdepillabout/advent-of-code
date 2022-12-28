module Main where

import Data.List ((!!), nub)
import ClassyPrelude hiding (maximumBy)
import Data.Coerce (coerce)
import Data.Foldable (maximumBy, Foldable)
import Data.Ord (comparing)
import Data.Vector ((!), generate)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

newtype TileId = TileId { unTileId :: Int }
  deriving (Eq, Ord, Show)

data Tile a = Tile TileId a
  deriving (Eq, Functor, Show)

data Pixel = PixDot | PixHash
  deriving (Eq, Show)

newtype RawTile p = RawTile (Vector (Vector p))
  deriving (Eq, Show)

newtype SideVal = SideVal Int
  deriving (Eq, Show)

data RawTileWithInfo p = RawTileWithInfo (RawTile p) [SideVal]
  deriving (Eq, Show)

data Flipped = NoFlipped | Flipped

data Rotated = NoRotated | Rotated90 | Rotated180 | Rotated270

data Puz = Puz TileId (RawTile Pixel) Flipped Rotated

data Board = Board (Vector (Vector Puz))

-- type MatchingPieces = Map TileId (Map Flipped (Map Rotated (NonEmptyList Puz)))

type AllPieces = Map TileId (RawTile Pixel)

data Side = Rght | Dwn

type Touching = Map (TileId, Flipped, Rotated, Side) TileId

data Position = Top | Bottom | Lft | Rgght

data ValForSide = ValForSide SideVal Position Flipped

type SideValMap = Map TileId [ValForSide]

-- type InverseSidevalMap = Map SideVal [TileId]

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
rawTileToSideVals :: RawTile Pixel -> [SideVal]
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

parseTiles :: Parser [Tile (RawTile Pixel)]
parseTiles = sepBy parseTile newline

rawTileGetInfo :: RawTile Pixel -> RawTileWithInfo Pixel
rawTileGetInfo rawTile = RawTileWithInfo rawTile $ rawTileToSideVals rawTile

rotateAndFlip :: Vector (Vector p) -> [Vector (Vector p)]
rotateAndFlip v =
  [ v
  , rotate90 v
  , rotate180 v
  , rotate90 $ rotate180 v
  , flipMat v
  , rotate90 $ flipMat v
  , rotate180 $ flipMat v
  , rotate90 $ rotate180 $ flipMat v
  ]

ll2vv :: [[a]] -> Vector (Vector a)
ll2vv = fromList . fmap fromList

-- |
-- >>> let raw1 = ll2vv [[1,2,3],[4,5,6]]
-- >>> let tile1 = Tile (TileId 1) $ RawTileWithInfo (RawTile raw1) []
-- >>> let raw2 = ll2vv [[7,8,9],[10,11,12]]
-- >>> let tile2 = Tile (TileId 2) $ RawTileWithInfo (RawTile raw2) []
-- >>> let raw3 = ll2vv [[13,14,15],[16,17,18]]
-- >>> let tile3 = Tile (TileId 3) $ RawTileWithInfo (RawTile raw3) []
-- >>> let total = pickPiece [tile1, tile2, tile3]
-- >>> length total
-- 24
pickPiece
  :: forall p
   . [Tile (RawTileWithInfo p)]
  -> [(Tile (RawTileWithInfo p), [Tile (RawTileWithInfo p)])]
pickPiece [] = []
pickPiece (h@(Tile tileId (RawTileWithInfo rawTile sideVals)) : ts) =
  fmap
    (\v -> (Tile tileId (RawTileWithInfo (RawTile v) sideVals), ts))
    (rotateAndFlip (coerce rawTile))
  <>
  fmap (\(a, x) -> (a, h:x)) (pickPiece ts)

-- |
-- >>> rotate90 $ ll2vv [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
-- [[4,8,12],[3,7,11],[2,6,10],[1,5,9]]
rotate90 :: forall a. Vector (Vector a) -> Vector (Vector a)
rotate90 v =
  let outerLen = length v
      innerLen = length $ headEx v

      f :: Int -> Vector a
      f row = -- value 0 to 3
        let g :: Int -> a
            g col = -- value 0 to 2
              (v ! col :: Vector a) ! (innerLen - row - 1)  :: a
        in generate outerLen g
  in generate innerLen f

-- |
-- >>> rotate180 $ fromList $ fmap fromList [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
-- [[12,11,10,9],[8,7,6,5],[4,3,2,1]]
rotate180 :: forall a. Vector (Vector a) -> Vector (Vector a)
rotate180 = rotate90 . rotate90

-- |
-- >>> flipMat $ ll2vv [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
-- [[4,3,2,1],[8,7,6,5],[12,11,10,9]]
flipMat :: forall a. Vector (Vector a) -> Vector (Vector a)
flipMat = fmap reverse

findTopRow
  :: [Tile (RawTileWithInfo Pixel)] -> [([Tile (RawTileWithInfo Pixel)], [Tile (RawTileWithInfo Pixel)])]
findTopRow inputTiles = do
  (piece, remainingPieces) <- pickPiece inputTiles
  undefined

sidesFor :: Vector (Vector Pixel) -> [ValForSide]
-- TODO: writing here
sidesFor vv =
  [ ValForSide  Top NoFlipped
  , ValForSide _ Lft NoFlipped
  , ValForSide _ Rgght NoFlipped
  , ValForSide _ Bottom NoFlipped
  ]

sideTopVec :: Vector (Vector a) -> Vector a
sideTopVec = headEx

sideRightTopToBottomVec :: Vector (Vector a) -> Vector a

createSideValMap :: [Tile (RawTile Pixel)] -> SideValMap
createSideValMap = go mempty
  where
  go :: SideValMap -> [Tile (RawTile Pixel)] -> SideValMap
  go m [] = m
  go m (Tile id (RawTile vv) : t) = go (insertMap id (sidesFor vv) m) t

solvePuzzle :: [Tile (RawTile Pixel)] -> [[Tile (RawTile Pixel)]]
solvePuzzle inputTiles@(Tile id r@(RawTile vecVec) : ts) =
  let allPuzId =
        setFromList $ fmap (\(Tile tileId _) -> tileId) inputTiles :: Set TileId
      sideValMap = createSideValMap inputTiles
  in
  traceShow (length allPuzId) $
  -- traceShow (length (nub (sort allPuzId))) $
  undefined
  -- let inputTilesWithSideVals = fmap (fmap rawTileGetInfo) inputTiles :: [Tile (RawTileWithInfo Pixel)]
  --     howManyOnSide = sqrt $ fromIntegral $ length inputTiles :: Float
  -- in
  -- traceShow howManyOnSide $ traceShow r $ traceShow (rawTileToSideVals r) undefined

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
