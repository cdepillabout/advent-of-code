module Main where

import ClassyPrelude hiding (many, optional)

import Data.Tree
import Text.Parsec
import Text.Parsec.Pos

digitsFollowedBySpaces :: Parsec ByteString () Int
digitsFollowedBySpaces = do
  digs <- many1 digit <* optional space
  let maybeInt = readMay digs
  case maybeInt of
    Just i -> pure i
    Nothing -> fail "not an int"

parseNums :: Parsec ByteString () [Int]
parseNums = many digitsFollowedBySpaces

data Metadatas = Metadatas [Int] deriving Show

parseTree :: Parsec [Int] () (Tree Metadatas)
parseTree = do
  traceM "starting parseTree..."
  numChildNodes <- singleToken
  traceM $ "\nin parseTree, got numChildNodes: " <> show numChildNodes
  numMetadata <- singleToken
  traceM $ "\nin parseTree, got numMetadata: " <> show numMetadata
  childrenNodes <- count numChildNodes parseTree
  metadata <- count numMetadata singleToken
  pure $ Node (Metadatas metadata) childrenNodes

singleToken :: Parsec [Int] () Int
singleToken =
  token show (\_ -> initialPos "testtest") Just

parseWholeTree :: Parsec [Int] () (Tree Metadatas)
parseWholeTree = parseTree

fromEith :: (e -> a) -> Either e a -> a
fromEith _ (Right a) = a
fromEith f (Left e) = f e

what :: Int -> [a] -> Maybe a
what _ [] = Nothing
what 0 (a:_) = Just a
what n (_:ts) = what (n - 1) ts

getyo :: [Int] -> Int -> Int
getyo children 0 = 0
getyo children n = fromMaybe 0 (what (n - 1) children)

yofoldwut :: Metadatas -> [Int] -> Int
yofoldwut (Metadatas metadatas) [] = sum metadatas
yofoldwut (Metadatas metadatas) children = sum $ fmap (getyo children) metadatas

main :: IO ()
main = do
  -- data_ <- readFile "day08-input-simple"
  data_ <- readFile "day08-input"
  print data_
  let parsedNums = fromEith (\e -> error $ "bad: " <> show e) $ runParser parseNums () "fefef" data_ :: [Int]
  print parsedNums
  let tree = fromEith (\e -> error "badabad") $ runParser parseWholeTree () "fefefefefe" parsedNums
  print tree
  -- print $ foldTree (\(Metadatas is) bs -> sum (is <> bs)) tree
  print $ foldTree yofoldwut tree
