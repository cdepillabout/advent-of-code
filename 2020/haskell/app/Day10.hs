
module Main where

import ClassyPrelude
import Data.Maybe (fromJust)

type NumOnes = Int
type NumThrees = Int

type OnesAndThrees = (NumOnes, NumThrees)

calcPotentialNext :: [Int] -> Map Int [Int]
calcPotentialNext = go mempty
  where
    go :: Map Int [Int] -> [Int] -> Map Int [Int]
    go accum = \case
      m : n : o : p : rest ->
        let is =
              catMaybes
                [ if n - m <= 3 then Just 1 else Nothing
                , if o - m <= 3 then Just 2 else Nothing
                , if p - m <= 3 then Just 3 else Nothing
                ]
        in go (insertMap m is accum) (n : o : p : rest)
      m : n : [o] ->
        let is =
              catMaybes
                [ if n - m <= 3 then Just 1 else Nothing
                , if o - m <= 3 then Just 2 else Nothing
                ]
        in go (insertMap m is accum) (n : [o])
      m : [n] ->
        let is =
              catMaybes
                [ if n - m <= 3 then Just 1 else Nothing
                ]
        in insertMap m is accum
      _ -> error "bad"

onesAndThrees :: [Int] -> OnesAndThrees
onesAndThrees = go (0, 0)
  where
    go :: OnesAndThrees -> [Int] -> OnesAndThrees
    go (accumOnes, accumThrees) = \case
      (n : m : rest) ->
        if m - n == 3
          then go (accumOnes, accumThrees + 1) (m : rest)
          else go (accumOnes + 1, accumThrees) (m : rest)
      _ -> (accumOnes, accumThrees)

allPaths :: Map Int [Int] -> [Int] -> Int
allPaths lookupTable is = go (zip is [0..])
  where
  go :: [(Int, Int)] -> Int
  go ((m, mIndex) : (n, nIndex) : (o, oIndex) : (p, pIndex) : rest) =
    let mNexts = {- trace ("allPaths, m: " <> show m) $ -} fromJust $ lookup m lookupTable
        oneNum = go ((n, nIndex) : (o, oIndex) : (p, pIndex) : rest)
        twoNum = go ((o, oIndex) : (p, pIndex) : rest)
        threeNum = go ((p, pIndex) : rest)
    in
    case mNexts of
      [1] -> oneNum
      [1,2] -> oneNum + twoNum
      [1,3] -> oneNum + threeNum
      [1,2,3] -> oneNum + twoNum + threeNum
      [2] -> twoNum
      [2,3] -> twoNum + threeNum
      [3] -> threeNum
  go ((m, mIndex) : (n, nIndex) : [(o, oIndex)]) = 1
  go ((n, nIndex) : [(o, oIndex)]) = 1

allPaths' :: Map Int [Int] -> [Int] -> (Map Int Int, Int)
allPaths' lookupTable is = go mempty (zip is [0..])
  where
  go :: Map Int Int -> [(Int, Int)] -> (Map Int Int, Int)
  go computedNumPaths ((m, mIndex) : (n, nIndex) : (o, oIndex) : (p, pIndex) : rest) =
    case lookup m computedNumPaths of
      Just paths -> (computedNumPaths, paths)
      Nothing ->
        let mNexts = {- trace ("allPaths, m: " <> show m) $ -} fromJust $ lookup m lookupTable
            (comp', oneNum) = go computedNumPaths ((n, nIndex) : (o, oIndex) : (p, pIndex) : rest)
            (comp'', twoNum) = go comp' ((o, oIndex) : (p, pIndex) : rest)
            (comp''', threeNum) = go comp'' ((p, pIndex) : rest)
            numPathsFromHere =
              case mNexts of
                [1] -> oneNum
                [1,2] -> oneNum + twoNum
                [1,3] -> oneNum + threeNum
                [1,2,3] -> oneNum + twoNum + threeNum
                [2] -> twoNum
                [2,3] -> twoNum + threeNum
                [3] -> threeNum
        in (insertMap m numPathsFromHere comp''', numPathsFromHere)
  go computedNumPaths ((m, mIndex) : (n, nIndex) : [(o, oIndex)]) =
    (insertMap m 1 computedNumPaths, 1)
  go computedNumPaths ((n, nIndex) : [(o, oIndex)]) =
    (insertMap n 1 computedNumPaths, 1)
  go computedNumPaths ([(o, oIndex)]) = (computedNumPaths, 0)

main :: IO ()
main = do
  file <- readFile "input-day10"
  -- file <- readFile "input-day10-example1"
  -- file <- readFile "input-day10-example2"
  let nums = sort $ fmap (fromJust . (readMay :: Text -> Maybe Int) :: Text -> Int) $ (lines $ decodeUtf8 file :: [Text])
  let realNums = (0 : nums ++ [maximum (impureNonNull nums) + 3])
  let potentNext = calcPotentialNext realNums
  print realNums
  print $ onesAndThrees realNums
  print $ potentNext
  print $ allPaths' potentNext realNums
