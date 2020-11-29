module Main where

import Control.Applicative ()
import Control.Lens
import Control.Monad (void)
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Map (Map, insertWith, toList)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector
import qualified Data.Vector as Vector
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MVector
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Debug.Trace

import Lib (someFunc)

main :: IO ()
main =
  Text.interact mySolutionPart2

type Parser = Parsec Void Text

-- |
--
-- >>> parseTest parseClaims "#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
-- [Claim {claimId = 2, leftEdge = 3, topEdge = 1, width = 4, height = 4},Claim {claimId = 3, leftEdge = 5, topEdge = 5, width = 2, height = 2}]
parseClaims :: Parser [Claim]
parseClaims =
  many $ do
    claim <- parseClaim
    void $ optional eol
    pure claim

-- |
--
-- >>> parseTest parseClaim "#2 @ 3,1: 4x4"
-- Claim {claimId = 2, leftEdge = 3, topEdge = 1, width = 4, height = 4}
parseClaim :: Parser Claim
parseClaim = do
  void $ char '#'
  claimId <- decimal
  void space1
  void $ char '@'
  void space1
  leftEdge <- decimal
  void $ char ','
  topEdge <- decimal
  void $ char ':'
  void space1
  width <- decimal
  void $ char 'x'
  height <- decimal
  pure Claim {..}

-- |
--
-- >>> convPointToIdx 4 5 2 1
-- 6
convPointToIdx :: Int -> Int -> Int -> Int -> Int
convPointToIdx totalWidth totalHeight x y = totalWidth * y + x

-- |
--
-- >>> convRowToIdx 4 5 2 1 2
-- [6,7]
convRowToIdx :: Int -> Int -> Int -> Int -> Int -> [Int]
convRowToIdx totalWidth totalHeight startingX startingY width =
  fmap (\i -> convPointToIdx totalWidth totalHeight (startingX + i) startingY) [0..(width - 1)]

-- |
--
-- >>> mkIndicies 5 6 2 3 3 2
-- [17,18,19,22,23,24]
--
-- >>> mkIndicies 4 5 0 0 2 5
-- [0,1,4,5,8,9,12,13,16,17]
mkIndicies :: Int -> Int -> Int -> Int -> Int -> Int -> [Int]
mkIndicies totalWidth totalHeight lEdge tEdge w h =
  let starting = convPointToIdx totalWidth totalHeight lEdge tEdge
  in Prelude.concatMap (\i -> convRowToIdx totalWidth totalHeight lEdge (tEdge + i) w) [0..(h - 1)]

indiciesForClaim :: Claim -> [Int]
indiciesForClaim Claim{..} = mkIndicies 1000 1000 leftEdge topEdge width height

updateVecWithClaim :: Claim -> MVector s (Set Int) -> ST s ()
updateVecWithClaim claim@Claim{..} vec = do
  let indicies = indiciesForClaim claim
  traverse_ (MVector.modify vec (Set.insert claimId)) indicies

data Claim = Claim
  { claimId :: Int
  , leftEdge :: Int
  , topEdge :: Int
  , width :: Int
  , height :: Int
  } deriving Show

updateVecWithClaims :: [Claim] -> MVector s (Set Int) -> ST s (MVector s (Set Int))
updateVecWithClaims [] vec = pure vec
updateVecWithClaims (c:cs) vec = updateVecWithClaim c vec *> updateVecWithClaims cs vec

mySolutionPart1 :: Text -> Text
mySolutionPart1 input =
  let claims = fromMaybe (error "could not parse") $ parseMaybe parseClaims input
      allLayout = create $ Data.Vector.Mutable.replicate (1000 * 1000) mempty >>= updateVecWithClaims claims :: Vector (Set Int)
      moreThanOne = Vector.filter (\s -> Set.size s > 1) allLayout
  in pack $ show $ Vector.length moreThanOne

indexOnlyForClaimId :: Vector (Set Int) -> Int -> Bool
indexOnlyForClaimId vec idx =
  let se = vec ! idx
  in Set.size se == 1

doesntOverlap :: Vector (Set Int) -> Claim -> Bool
doesntOverlap vec claim@Claim{..} =
  let indicies = indiciesForClaim claim
  in Prelude.all (indexOnlyForClaimId vec) indicies

mySolutionPart2 :: Text -> Text
mySolutionPart2 input =
  let claims = fromMaybe (error "could not parse") $ parseMaybe parseClaims input
      allLayout = create $ Data.Vector.Mutable.replicate (1000 * 1000) mempty >>= updateVecWithClaims claims :: Vector (Set Int)
      maybeUnoverlappingClaim = List.find (doesntOverlap allLayout) claims
  in pack $ show $ fromMaybe (error "couldn't find") maybeUnoverlappingClaim
