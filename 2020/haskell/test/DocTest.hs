module Main where

import Build_doctests (Component(..), Name(..), components)
import Data.Foldable (for_)
import Test.DocTest (doctest)

nameToString :: Name -> String
nameToString (NameLib Nothing) = "library advent-of-code2018"
nameToString (NameLib (Just lib)) = "library " ++ lib
nameToString (NameExe exe) = "executable " ++ exe

main :: IO ()
main = do
  putStrLn "\n"
  for_ components $ \(Component name flags pkgs sources) -> do
    putStrLn $ nameToString name ++ ":"
    let args = flags ++ pkgs ++ sources
    -- for_ args putStrLn
    putStr "\t"
    doctest args
    putStrLn ""
