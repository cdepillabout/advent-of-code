
module Main where

-- import Control.Lens (TraversableWithIndex, view, set, itraverse, itraversed)
import Control.Lens -- (TraversableWithIndex, view, set, itraverse, itraversed)
import qualified Control.Lens as Lens
import ClassyPrelude
import Data.Maybe (fromJust)
import Data.List ((!!))

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

newtype Toogle f b a = Toogle { unToogle :: f (Toogle f b b) -> f (Maybe b) -> IO (a, f (Maybe b)) }
  deriving Functor

instance Applicative (Toogle f b) where
  pure a = Toogle $ \_ nulls -> pure (a, nulls)
  Toogle f <*> Toogle g = Toogle $ \r nulls -> do
    (a2b, null') <- f r nulls
    (b, null'') <- g r null'
    pure (a2b b, null'')

instance Monad (Toogle f b) where
  Toogle f >>= k = Toogle $ \r nulls -> do
    (a, nulls') <- f r nulls
    unToogle (k a) r nulls'

instance MonadIO (Toogle f b) where
  liftIO f = Toogle $ \_ nulls -> do
    res <- f
    pure (res, nulls)

toogleHitchAt
  :: forall b f i
   . ( Lens.Index (f (Maybe b)) ~ i
     , Lens.Index (f (Toogle f b b)) ~ i
     , Lens.IxValue (f (Maybe b)) ~ Maybe b
     , Lens.IxValue (f (Toogle f b b)) ~ Toogle f b b
     , forall x. Ixed (f x)
     )
  => i
  -> Toogle f b b
toogleHitchAt i = Toogle go
  where
    go :: f (Toogle f b b) -> f (Maybe b) -> IO (b, f (Maybe b))
    go r nulls =
      case fromJust $ nulls ^? ix i of
        Just val -> pure (val, nulls)
        Nothing -> do
          let toogFunc = unToogle $ fromJust (r ^? ix i)
          (b, nulls') <- toogFunc r nulls
          pure (b, set (ix i) (Just b) nulls')

-- toogleHitchAt :: forall b f. (forall g x. Functor g => (x -> g x) -> f x -> g (f x)) -> Toogle f b b
-- toogleHitchAt fLens = Toogle go
--   where
--     go :: f (Toogle f b b) -> f (Maybe b) -> IO (b, f (Maybe b))
--     go r nulls =
--       case view fLens nulls of
--         Just val -> pure (val, nulls)
--         Nothing -> do
--           let toogFunc = unToogle $ view fLens r
--           (b, nulls') <- toogFunc r nulls
--           pure (b, set fLens (Just b) nulls')

runToogles
  :: forall b f i
   . ( TraversableWithIndex i f
     , Lens.Index (f (Maybe b)) ~ i
     , Lens.Index (f (Toogle f b b)) ~ i
     , Lens.IxValue (f (Maybe b)) ~ Maybe b
     , Lens.IxValue (f (Toogle f b b)) ~ Toogle f b b
     , forall x. Ixed (f x)
     , Show i
     )
  => (forall x. i -> f x -> x)
  -> f (Toogle f b b)
  -> IO (f b)
runToogles getter toogles = do
  let toogle = itraverse go toogles :: Toogle f b (f b)
      m = unToogle toogle :: f (Toogle f b b) -> f (Maybe b) -> IO (f b, f (Maybe b))
      nothings = fmap (const Nothing) toogles
  (fb, _) <- m toogles nothings :: IO (f b, f (Maybe b))
  pure fb
  where
    go :: i -> Toogle f b b -> Toogle f b b
    go i (Toogle _inner) = do
      putStrLn $ "In runToogles, go, on index i: " <> tshow i
      toogleHitchAt i

example7 :: IO ()
example7 = do
  res <- runToogles lookupElem (zipWith ($) [x0, x1, x2, x3, x4, x5, x6] [0..])
  print res
  where
    lookupElem :: Int -> [a] -> a
    lookupElem i = (!! i)

    -- setElem :: Int -> a -> [a] -> [a]
    -- setElem i a = set (ix i) a

    -- TODO: Figure out how to abstract lookupElem and setElem
    -- (possibly with TraversableWithIndex and Ixed?)

    x0 :: Int -> Toogle [] String String
    x0 i = do
      liftIO $ putStrLn "Evaluating x0, about to pull out x1"
      x1Val <- toogleHitchAt 1
      liftIO $ putStrLn "Evaluating x0, finished pulling out x0"
      pure $ "x0 val " <> x1Val

    x1 :: Int -> Toogle [] String String
    x1 i = do
      liftIO $ putStrLn "Evaluating x1, about to pull out x3"
      x3Val <- toogleHitchAt 3
      liftIO $ putStrLn "Evaluating x1, finished pulling out x3"
      pure $ "X1 (" <> x3Val <> ") VAL"

    x2 :: Int -> Toogle [] String String
    x2 _i = do
      liftIO $ putStrLn "Evaluating x2"
      pure "x2 val"

    x3 :: Int -> Toogle [] String String
    x3 _i = do
      liftIO $ putStrLn "Evaluating x3"
      pure "x3 val"

    x4 :: Int -> Toogle [] String String
    x4 _i = do
      liftIO $ putStrLn "Evaluating x4"
      pure "x4 val"

    x5 :: Int -> Toogle [] String String
    x5 _i = do
      liftIO $ putStrLn "Evaluating x5, about to pull out x3"
      x3Val <- toogleHitchAt 3
      liftIO $ putStrLn "Evaluating x5, finished pulling out x3"
      pure $ "X5 (" <> x3Val <> ") VAL"

    x6 :: Int -> Toogle [] String String
    x6 _i = do
      liftIO $ putStrLn "Evaluating x6, about to pull out x5"
      x5Val <- toogleHitchAt 5
      liftIO $ putStrLn "Evaluating x6, finished pulling out x5"
      pure $ "X6 (" <> x5Val <> ") VAL"

allPaths'' :: Map Int [Int] -> [Int] -> (Map Int Int, Int)
allPaths'' lookupTable is = undefined -- runTangle
  -- go mempty (zip is [0..])
  -- where
  -- go :: Map Int Int -> [(Int, Int)] -> (Map Int Int, Int)
  -- go computedNumPaths ((m, mIndex) : (n, nIndex) : (o, oIndex) : (p, pIndex) : rest) =
  --   case lookup m computedNumPaths of
  --     Just paths -> (computedNumPaths, paths)
  --     Nothing ->
  --       let mNexts = {- trace ("allPaths, m: " <> show m) $ -} fromJust $ lookup m lookupTable
  --           (comp', oneNum) = go computedNumPaths ((n, nIndex) : (o, oIndex) : (p, pIndex) : rest)
  --           (comp'', twoNum) = go comp' ((o, oIndex) : (p, pIndex) : rest)
  --           (comp''', threeNum) = go comp'' ((p, pIndex) : rest)
  --           numPathsFromHere =
  --             case mNexts of
  --               [1] -> oneNum
  --               [1,2] -> oneNum + twoNum
  --               [1,3] -> oneNum + threeNum
  --               [1,2,3] -> oneNum + twoNum + threeNum
  --               [2] -> twoNum
  --               [2,3] -> twoNum + threeNum
  --               [3] -> threeNum
  --       in (insertMap m numPathsFromHere comp''', numPathsFromHere)
  -- go computedNumPaths ((m, mIndex) : (n, nIndex) : [(o, oIndex)]) =
  --   (insertMap m 1 computedNumPaths, 1)
  -- go computedNumPaths ((n, nIndex) : [(o, oIndex)]) =
  --   (insertMap n 1 computedNumPaths, 1)
  -- go computedNumPaths ([(o, oIndex)]) = (computedNumPaths, 0)


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
