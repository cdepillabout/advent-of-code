module Main where

import Prelude

type PubKey = Integer
type LoopSize = Int
type SubjectNum = Integer
type Val = Integer
type EncKey = Integer

findLoopSize :: SubjectNum -> PubKey -> LoopSize
findLoopSize subjNum pubKey = f 1 0
  where
  f :: Val -> LoopSize -> LoopSize
  f val currLoopSize =
    let newVal = (subjNum * val) `mod` 20201227
    in
    if pubKey == newVal
      then currLoopSize + 1
      else f newVal (currLoopSize + 1)

calcEncKey :: SubjectNum -> LoopSize -> EncKey
calcEncKey subjNum loopSize = f 1 0
  where
  f :: Val -> LoopSize -> Val
  f val currLoopSize
    | currLoopSize == loopSize = val
    | otherwise =
      let newVal = (subjNum * val) `mod` 20201227
      in f newVal (currLoopSize + 1)

main :: IO ()
main = do
  let initialSubjNum = 7
      -- (cardPubKey, doorPubKey) = (5764801, 17807724)
      (cardPubKey, doorPubKey) = (17607508, 15065270)
      cardLoopSize = findLoopSize initialSubjNum cardPubKey
      doorLoopSize = findLoopSize initialSubjNum doorPubKey
      doorEncKey = calcEncKey doorPubKey (fromIntegral cardLoopSize)
      cardEncKey = calcEncKey cardPubKey (fromIntegral doorLoopSize)
  putStrLn $ "cardLoopSize: " <> show cardLoopSize
  putStrLn $ "doorLoopSize: " <> show doorLoopSize
  putStrLn $ "cardEncKey: " <> show cardEncKey
  putStrLn $ "doorEncKey: " <> show doorEncKey
