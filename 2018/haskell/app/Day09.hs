module Main where

import ClassyPrelude

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List ((!!))

type Players = Int
type Marbles = Int
type HighScore = Int

type Player = Int

-- type Board = [Int]
type Board = Seq Int

type PrevMarbleNumIdx = Int
type NewMarbleNum = Int

-- insertAt :: Int -> a -> [a] -> [a]
-- insertAt 0 a as = a : as
-- insertAt n a (h:t) = h : insertAt (n - 1) a t

type NewMarbleIdx = Int

insertMarble :: PrevMarbleNumIdx -> NewMarbleNum -> Board -> (NewMarbleIdx, Board)
insertMarble prevMarbleNumIdx newMarbleNum board =
  let indexOfNext = (prevMarbleNumIdx + 2) `mod` length board
  in (indexOfNext, Seq.insertAt indexOfNext newMarbleNum board)

type Score = Int
type Scores = Map Player Score

type NewCurrentMarble = Int

type MarbleRemovedNum = Int

-- removeIndex :: Int -> Board -> (MarbleRemovedNum, Board)
-- removeIndex 0 (h:t) = (h, t)
-- removeIndex n (h:t) = let (ff, tt) = removeIndex (n - 1) t in (ff, h:tt)

removeIndex :: Int -> Board -> (MarbleRemovedNum, Board)
removeIndex i b = (Seq.index b i, Seq.deleteAt i b)

updateScores :: Player -> Score -> Scores -> Scores
updateScores p s scores = insertWith (+) p s scores

do23
  :: Players
  -> Scores
  -> Board
  -> PrevMarbleNumIdx
  -> NewMarbleNum
  -> (Scores, Board, NewCurrentMarble)
do23 players scores board prevMarbleNumIdx newMarbleNum =
  let marbleToRemoveIdx = (prevMarbleNumIdx - 7) `mod` length board
      (removedMarbleNum, newBoard) = removeIndex marbleToRemoveIdx board
      scoreToAdd = newMarbleNum + removedMarbleNum
      playerToAddScoreTo = newMarbleNum `mod` players
      newScores = updateScores playerToAddScoreTo scoreToAdd scores
  in
  -- trace (
  --   "do23, players: " <> show players <>
  --   " scores: " <> show scores <>
  --   " board: " <> show board <>
  --   " prevMarbleNum: " <> show prevMarbleNum <>
  --   " newMarbleNum: " <> show newMarbleNum <>
  --   " prevMarbleNumIdx: " <> show prevMarbleNumIdx <>
  --   " marbleToRemoveIdx: " <> show marbleToRemoveIdx
  --   )
  (newScores, newBoard, marbleToRemoveIdx)

createBoard :: Players -> Marbles -> (Scores, Board)
createBoard players lastMarble = go Map.empty (Seq.singleton 0) 0 1
  where
  go :: Scores -> Board -> PrevMarbleNumIdx -> NewMarbleNum -> (Scores, Board)
  go scores _ 0 1 = go scores (0 Seq.<| Seq.singleton 1) 1 2
  go scores !board !prevMarbleNumIdx !newMarbleNum
    | newMarbleNum > lastMarble = (scores, board)
    | newMarbleNum `mod` 23 == 0 =
        let (newScores, newBoard, newCurrentMarbleIdx) =
              do23 players scores board prevMarbleNumIdx newMarbleNum
        in go newScores newBoard newCurrentMarbleIdx (newMarbleNum + 1)
    | otherwise =
        let (newMarbleIdx, newBoard) =
              insertMarble prevMarbleNumIdx newMarbleNum board
        in
        go
          scores
          newBoard
          newMarbleIdx
          (newMarbleNum + 1)

main :: IO ()
main = do
  -- let players = 9
  --     lastMarble = 25
  let players = 424
      lastMarble = 7148200
  -- let players = 405
  --     lastMarble = 71700
      -- highScore = simGame [0]
      (scores, _board) = createBoard players lastMarble
  print (lastEx $ sort $ Map.elems scores)
  pure ()
