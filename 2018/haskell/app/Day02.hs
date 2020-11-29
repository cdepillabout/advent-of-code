module Main where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Map (Map, insertWith, toList)
import Data.Text (Text)
import qualified Data.Text as Text

import Debug.Trace

import Lib (someFunc)

main :: IO ()
main =
  interact mySolutionPart2

findAllCounts :: String -> Map Char Int
findAllCounts = foldr (\a -> insertWith (+) a 1) mempty

findNum :: Int -> Map Char Int -> Int
findNum num ma =
  case filter (== num) . fmap snd $ toList ma of
    [] -> 0
    _ -> 1

getCounts :: String -> (Int, Int)
getCounts s =
  let aCounts = findAllCounts s
  in (findNum 2 aCounts, findNum 3 aCounts)

totalCount :: [(Int, Int)] -> Int
totalCount is =
  let (totalTwo, totalThree) = foldr (\(two, three) (accTwo, accThree) -> (accTwo + two, accThree + three)) (0, 0) is
  in totalTwo * totalThree

mySolutionPart1 :: String -> String
mySolutionPart1 inputStr =
  let l = lines inputStr
  in show $ totalCount $ fmap getCounts l

isSimChar :: Char -> Char -> Sim
isSimChar c d | c == d = Same
              | otherwise = Diff

data Sim = Diff | Same deriving Eq

isSimilar :: String -> String -> Bool
isSimilar str str' = oneDiff $ zipWith isSimChar str str'

oneDiff :: [Sim] -> Bool
oneDiff sims = length (filter (== Diff) sims) == 1

-- |
--
-- >>> findSimilar' "hello" ["badje", "iello"]
-- Just "iello"
--
-- >>> findSimilar' "hello" ["gello", "dodii"]
-- Just "gello"
--
-- >>> findSimilar' "hello" ["hello", "byeee"]
-- Nothing
--
-- >>> findSimilar' "hello" ["toooo", "byeee"]
-- Nothing
findSimilar' :: String -> [String] -> Maybe String
findSimilar' _ [] = Nothing
findSimilar' str (h:ts) = if isSimilar str h then Just h else findSimilar' str ts

-- |
--
-- >>> removeDiffChar "hello" "gello"
-- "ello"
--
-- >>> removeDiffChar "hello" "hdllo"
-- "hllo"
removeDiffChar :: String -> String -> String
removeDiffChar [] [] = []
removeDiffChar (h:ts) (h':ts')
  | h == h' = h : removeDiffChar ts ts'
  | otherwise = removeDiffChar ts ts'
removeDiffChar _ _ = error "removeDiffChar: called with two strings of different lengths"

-- |
--
-- >>> findSimilar ["apple", "badje", "bpple"]
-- Just "pple"
--
-- >>> findSimilar ["badje", "turtl", "ttrtl"]
-- Just "trtl"
--
-- >>> findSimilar ["one", "two", "aaa"]
-- Nothing
findSimilar :: [String] -> Maybe String
findSimilar [] = Nothing
findSimilar (h:ts) =
  case findSimilar' h ts of
    Nothing -> findSimilar ts
    Just sim -> Just $ removeDiffChar h sim

-- |
--
-- >>> mySolutionPart2 "apple\nradde\npbdde\nraede\noafe\n"
-- "rade"
mySolutionPart2 :: String -> String
mySolutionPart2 inputStr =
  let l = lines inputStr
  in maybe (error "mySolutionPart2: didn't find any similar strings") id $ findSimilar l -- show $ totalCount $ fmap getCounts l
