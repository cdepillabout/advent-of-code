module Main where

import Data.List ((!!))
import ClassyPrelude hiding (maximumBy)
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)


-- import qualified Data.Sequence as Seq
-- import Data.Sequence (Seq)

-- import qualified Data.Map as Map
-- import Data.Map (Map)

-- import Data.List ((!!))

-- |
--
-- >>> powerLevel 8 3 5
-- 4
-- >>> powerLevel 57 122 79
-- -5
-- >>> powerLevel 39 217 196
-- 0
-- >>> powerLevel 71 101 153
-- 4
-- >>> powerLevel serialNum 0 0
-- 3
powerLevel
  :: Int -- ^ serial number
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int
powerLevel serialNum x y =
  let rackId = x + 10
      power = rackId * y
      power' = power + serialNum
      power'' = power' * rackId
      powerStr = take 1 $ drop 2 $ reverse $ show power''
  in
  case powerStr of
    (p:_) -> read [p] - 5
    [] -> -5
  where
    read x = fromMaybe (undefined) (readMay x)

type PowerLevel = Int
type X = Int
type Y = Int

serialNum ::Int
serialNum = 1788

allPowerLevels :: [[PowerLevel]]
allPowerLevels =
  flip fmap [0..299] $ \x ->
    flip fmap [0..299] $ \y ->
      powerLevel serialNum x y

allPowerLevels :: Vector (Vector PowerLevel)
allPowerLevels = undefined
  -- forM [0..299] $ \x ->
  --   forM [0..299] $ \y ->
  --     powerLevel serialNum x y

sum3x3 :: [[(X,Y,PowerLevel)]]
sum3x3 =
  flip fmap [0..297] $ \x ->
    flip fmap [0..297] $ \y ->
      let x0y0 = (allPowerLevels !! (x + 0)) !! (y + 0) :: PowerLevel
          x0y1 = (allPowerLevels !! (x + 0)) !! (y + 1)
          x0y2 = (allPowerLevels !! (x + 0)) !! (y + 2)
          x1y0 = (allPowerLevels !! (x + 1)) !! (y + 0)
          x1y1 = (allPowerLevels !! (x + 1)) !! (y + 1)
          x1y2 = (allPowerLevels !! (x + 1)) !! (y + 2)
          x2y0 = (allPowerLevels !! (x + 2)) !! (y + 0)
          x2y1 = (allPowerLevels !! (x + 2)) !! (y + 1)
          x2y2 = (allPowerLevels !! (x + 2)) !! (y + 2)
          s =
            sum
              [ x0y0
              , x0y1
              , x0y2
              , x1y0
              , x1y1
              , x1y2
              , x2y0
              , x2y1
              , x2y2
              ]
      in
      (x,y,s)
      -- traceShow (x,y) $

thrd :: (a,b,c) -> c
thrd (a,b,c) = c

thrd' :: (a,b,c,d) -> c
thrd' (a,b,c,d) = c

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

-- |
--
-- >>> max3x3
-- (235,35,31)
max3x3 :: (X, Y, PowerLevel)
max3x3 = maxBy thrd $ (fmap (maxBy thrd) sum3x3 :: [(X, Y, PowerLevel)])

type SquareSize = Int

sum3x3Better :: SquareSize -> [[(X,Y,PowerLevel,SquareSize)]]
sum3x3Better squareSize =
    flip fmap [0..(300 - squareSize)] $ \startingX ->
      flip fmap [0..(300 - squareSize)] $ \startingY ->
        let lalala =
              flip fmap [startingX..(startingX + squareSize - 1)] \x ->
                flip fmap [startingY..(startingY + squareSize - 1)] \y ->
                  allPowerLevels !! x !! y
            fjfjfsum = sum $ fmap sum lalala
        in (startingX, startingY, fjfjfsum, squareSize)

-- |
--
-- >>> max3x3Better 3
-- (235,35,31,3)
max3x3Better :: SquareSize -> (X,Y,PowerLevel,SquareSize)
max3x3Better squareSize =
  maxBy thrd' $ fmap (maxBy thrd') (sum3x3Better squareSize)

sum3x3Better' :: [[[(X, Y, PowerLevel, SquareSize)]]]
sum3x3Better' =
  flip fmap [1..300] $ \squareSize ->
    sum3x3Better squareSize

type Ga = (X,Y,PowerLevel,SquareSize)

max3x3Better' :: (X,Y,PowerLevel,SquareSize)
max3x3Better' =
  -- maxBy thrd' $ fmap (maxBy thrd') sum3x3Better'
  -- maxBy thrd' $ fmap (maxBy thrd') sum3x3Better'
  maxBy thrd' $ fmap g sum3x3Better'
  where
    g :: [[Ga]] -> Ga
    g ggg = maxBy thrd' $ fmap h ggg

    h :: [Ga] -> Ga
    h = maxBy thrd'

main :: IO ()
main = do
  print sum3x3Better'
  -- print max3x3Better'
  pure ()
