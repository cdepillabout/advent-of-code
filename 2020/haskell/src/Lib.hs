module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Here's an example of using doctest:
--
-- >>> exampleFunction 3
-- 4
exampleFunction :: Int -> Int
exampleFunction i = i + 1
