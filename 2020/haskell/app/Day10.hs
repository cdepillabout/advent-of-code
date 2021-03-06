{-# LANGUAGE UndecidableInstances #-}

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
  go ((m, _mIndex) : (n, nIndex) : (o, oIndex) : (p, pIndex) : rest) =
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
      _ -> error "this should never happen"
  go ((_m, _mIndex) : (_n, _nIndex) : [(_o, _oIndex)]) = 1
  go ((_n, _nIndex) : [(_o, _oIndex)]) = 1
  go _ = 1

allPaths' :: Map Int [Int] -> [Int] -> (Map Int Int, Int)
allPaths' lookupTable is = go mempty (zip is [0..])
  where
  go :: Map Int Int -> [(Int, Int)] -> (Map Int Int, Int)
  go computedNumPaths ((m, _mIndex) : (n, nIndex) : (o, oIndex) : (p, pIndex) : rest) =
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
                _ -> error "this should never happen"
        in (insertMap m numPathsFromHere comp''', numPathsFromHere)
  go computedNumPaths ((m, _mIndex) : (_n, _nIndex) : [(_o, _oIndex)]) =
    (insertMap m 1 computedNumPaths, 1)
  go computedNumPaths ((n, _nIndex) : [(_o, _oIndex)]) =
    (insertMap n 1 computedNumPaths, 1)
  go computedNumPaths ([(_o, _oIndex)]) = (computedNumPaths, 0)
  go _ _ = error "bad bad"

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

-- toogleHitchAt
--   :: forall b f i
--    . ( Lens.Index (f (Maybe b)) ~ i
--      , Lens.Index (f (Toogle f b b)) ~ i
--      , Lens.IxValue (f (Maybe b)) ~ Maybe b
--      , Lens.IxValue (f (Toogle f b b)) ~ Toogle f b b
--      , forall x. Ixed (f x)
--      )
--   => i
--   -> Toogle f b b
-- toogleHitchAt i = Toogle go
--   where
--     go :: f (Toogle f b b) -> f (Maybe b) -> IO (b, f (Maybe b))
--     go r nulls =
--       case fromJust $ nulls ^? ix i of
--         Just val -> pure (val, nulls)
--         Nothing -> do
--           let toogFunc = unToogle $ fromJust (r ^? ix i)
--           (b, nulls') <- toogFunc r nulls
--           pure (b, set (ix i) (Just b) nulls')

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

-- TODO: Write this to just take a Traversal'?
toogleHitchAt
  :: forall b f
   . (forall x. Traversal' (f x) x)
  -> Toogle f b (Maybe b)
toogleHitchAt lookupL = Toogle go
  where
    go :: f (Toogle f b b) -> f (Maybe b) -> IO (Maybe b, f (Maybe b))
    go r nulls =
      case preview lookupL nulls of
        -- The lookup traversal doesn't actually point to a valid element in
        -- the nulls data structure.
        Nothing -> pure (Nothing, nulls)
        -- The lookup traversal points to an already computed value in the
        -- nulls data structure.  Just return this.
        Just (Just val) -> pure (Just val, nulls)
        -- The lookup traversal points to a valid value in the nulls data
        -- structure, but it hasn't been computed yet.  Look it up.
        Just Nothing -> do
          case preview lookupL r of
            -- The lookup traversal doesn't actually point to a valid element
            -- in the r data structure (even though it does in the nulls data
            -- structure).  This is strange. In runToogle we know that
            -- nulls has the same length as r, so any traversal that returns
            -- a value for nulls should also return a value for r.
            Nothing -> pure (Nothing, nulls)
            Just toogFunc -> do
              (b, nulls') <- unToogle toogFunc r nulls
              pure (Just b, set lookupL (Just b) nulls')

unsafeToogleHitchAt
  :: forall b f
   . (forall x. Traversal' (f x) x)
  -> Toogle f b b
unsafeToogleHitchAt lookupL = fmap fromJust $ toogleHitchAt lookupL

-- runToogles
--   :: forall b f i
--    . ( TraversableWithIndex i f
--      , Lens.Index (f x) ~ i
--      , Lens.IxValue (f (Maybe b)) ~ Maybe b
--      , Lens.IxValue (f (Toogle f b b)) ~ Toogle f b b
--      , forall x. Ixed (f x)
--      , Show i
--      )
--   => f (Toogle f b b)
--   -> IO (f b)
runToogles
  :: forall b f i
   . ( TraversableWithIndex i f
     , TraversableIndexed i f
     , Show i
     )
  => f (Toogle f b b)
  -> IO (f b)
runToogles toogles = do
  let toogle = itraverse go toogles :: Toogle f b (f b)
      m = unToogle toogle :: f (Toogle f b b) -> f (Maybe b) -> IO (f b, f (Maybe b))
      nothings = fmap (const Nothing) toogles
  (fb, _) <- m toogles nothings :: IO (f b, f (Maybe b))
  pure fb
  where
    go :: i -> Toogle f b b -> Toogle f b b
    go i (Toogle _inner) = do
      putStrLn $ "In runToogles, go, on index i: " <> tshow i
      -- We know that all our indexes are valid, because it is implied by
      -- itraverse.
      unsafeToogleHitchAt (travIx i)

-- type family Index (s :: *) :: *
-- type instance Index [a] = Int

-- type family IxValue (m :: *) :: *
-- type instance IxValue [a] = a

-- class Ixed m where
--   ix :: Index m -> Traversal' m (IxValue m)

class TraversableIndexed i t | t -> i where
  travIx :: forall x. i -> Traversal' (t x) x

instance TraversableIndexed Int [] where
  travIx :: forall x. Int -> Traversal' [x] x
  travIx i = ix i

instance Ord k => TraversableIndexed k (Map k) where
  travIx :: forall v. k -> Traversal' (Map k v) v
  travIx k = ix k

-- instance Ixed (t i) => TraversableIndexed i t where
--   travIx :: forall x. i -> Traversal' (t x) x
--   travIx = ix

example7 :: IO ()
example7 = do
  res <- runToogles (zipWith ($) [x0, x1, x2, x3, x4, x5, x6] [0..])
  print res
  where
    x0 :: Int -> Toogle [] String String
    x0 _i = do
      liftIO $ putStrLn "Evaluating x0, about to pull out x1"
      x1Val <- unsafeToogleHitchAt (ix 1)
      liftIO $ putStrLn "Evaluating x0, finished pulling out x0"
      pure $ "x0 val " <> x1Val

    x1 :: Int -> Toogle [] String String
    x1 _i = do
      liftIO $ putStrLn "Evaluating x1, about to pull out x3"
      x3Val <- unsafeToogleHitchAt (ix 3)
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
      x3Val <- unsafeToogleHitchAt (ix 3)
      liftIO $ putStrLn "Evaluating x5, finished pulling out x3"
      pure $ "X5 (" <> x3Val <> ") VAL"

    x6 :: Int -> Toogle [] String String
    x6 _i = do
      liftIO $ putStrLn "Evaluating x6, about to pull out x5"
      x5Val <- unsafeToogleHitchAt (ix 5)
      liftIO $ putStrLn "Evaluating x6, finished pulling out x5"
      pure $ "X6 (" <> x5Val <> ") VAL"

allPaths'' :: Map Int [Int] -> [Int] -> IO (Map Int Int)
allPaths'' lookupTable is =
  let toogMap :: Map Int (Toogle (Map Int) Int Int)
      toogMap = mapFromList $ zip [0..] $ fmap toogles $ zip [0..] is
  in
  runToogles toogMap
  where
  toogles :: (Int, Int) -> Toogle (Map Int) Int Int
  toogles (i,m) = do
    oneNum <- toogleHitchAt $ ix (i + 1)
    twoNum <- toogleHitchAt $ ix (i + 2)
    threeNum <- toogleHitchAt $ ix (i + 3)
    case (oneNum, twoNum, threeNum) of
      (Nothing, _, _) -> pure 0
      (Just _, Nothing, _) -> pure 1
      (Just _, Just _, Nothing) -> pure 1
      (Just one, Just two, Just three) -> do
        let mNexts = fromJust $ lookup m lookupTable
            numPathsFromHere =
              case mNexts of
                [1] -> one
                [1,2] -> one + two
                [1,3] -> one + three
                [1,2,3] -> one + two + three
                [2] -> two
                [2,3] -> two + three
                [3] -> three
                _ -> error "This should never happen"
        pure numPathsFromHere

main :: IO ()
main = do
  file <- readFile "input-day10"
  -- file <- readFile "input-day10-example1"
  -- file <- readFile "input-day10-example2"
  let nums = sort $ fmap (fromJust . (readMay :: Text -> Maybe Int) :: Text -> Int) $ (lines $ decodeUtf8 file :: [Text])
  let realNums = (0 : nums ++ [maximum (impureNonNull nums) + 3])
  let potentNext = calcPotentialNext realNums
  print realNums
  -- print $ onesAndThrees realNums
  print $ potentNext
  -- print $ allPaths' potentNext realNums
  paths <- allPaths'' potentNext realNums
  print paths

-----------------------------------


-- newtype TangleT b f m a = TangleT
--   { unTangleT
--       :: RWST
--            (f (TangleT b m b))
--            ()
--            (f (Maybe b))
--            -- (f (STRef (Maybe b)))
--            m
--            a
--   }

-- runTangleT
--   :: f (TangleT b f m b)
--   -> m (f b)
-- runTangleT tangles =
--   let x = itraverse go tangles :: TangleT b f m (f b)
--   where
--     go :: i -> TangleT f b b -> TangleT f b b
--     go i t = hitchAt _

-- hitchAt
--   :: (forall z. Traversal' (f z) z)
--   -> TangleT b f m (Maybe b)
-- hitchAt t = TangleT $ do
--   fMaybeB <- get
--   case fMaybeB ^? t of
--     Nothing -> undefined
--     Just maybeB ->
--       case maybeB of
--         Nothing -> do
--           fTangleT <- ask
  

-- example10 :: IO [Int]
-- example10 = do
--   runTangleT tangles
--   where
--     tangles :: [TangleT Int [] IO Int]
--     tangles =
--       [ pure 10
--       , do
--           firstElem <- hitchAt (ix 0)
--           thirdElem <- hitchAt 2
--           pure $ firstElem + thirdElem
--       , read <$> getLine
--       , do
--          secondElem <- hitchAt 1
--          pure $ secondElem + 10
--       ]

-- IO [ 10, 20, 30 ]

-- Could you automatically produce a dependency graph?
-- Have additional state like a Map that shows which nodes
-- accessed which other nodes?

-- Jonas suggests to have a clear reason why you don't just
-- use a self-referential data structure.
-- possible reason: an exotic monad transformer in m, or
-- dependency tracking.

-- When building a list, it may get very expensive to
-- do indexing with hitchAt.  So you could instead
-- keep a Map of index to value in the state, so that
-- hitchAt would be really fast.

-- What is the relationship between TangleT and MonadFix?

-- Can you do Nixpkgs-style overlays with TangleT?
