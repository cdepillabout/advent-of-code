{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MyLens where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(ask))
import Data.Functor.Contravariant


-- data Lens s a = Lens
--   { getter :: s -> a
--   , setter :: s -> a -> s
--   }

-- userGlucoseLens :: Lens User Int
-- userGlucoseLens = Lens
--   { getter = \user -> glucoseLevel user
--   , setter =
--       \user newGlucose ->
--         user { glucoseLevel = newGlucose }
--   }

-- ericsGlucoseLevel :: Int
-- ericsGlucoseLevel = (getter userGlucoseLens) eric

-- setEricsGlucose :: User
-- setEricsGlucose = (setter userGlucoseLens) eric 100

-- userNameLens :: Lens User Name
-- userNameLens = Lens
--   { getter = \user -> userName user
--   , setter =
--       \user newFirstName ->
--         user { userName = newFirstName }
--   }

-- nameFirstLens :: Lens Name String
-- nameFirstLens = Lens
--   { getter = \name -> nameFirst name
--   , setter =
--       \name newFirstName ->
--         name { nameFirst = newFirstName }
--   }

-- composeLens :: Lens s a -> Lens a b -> Lens s b
-- composeLens lensSA lensAB = Lens
--   { getter = \s ->
--       let a = (getter lensSA) s
--       in (getter lensAB) a
--   , setter = \s b ->
--       let oldA = (getter lensSA) s
--           newA = (setter lensAB) oldA b
--       in (setter lensSA) s newA
--   }

-- userFirstNameLens :: Lens User String
-- userFirstNameLens = composeLens userNameLens nameFirstLens

-- ericFirstName :: String
-- ericFirstName = (getter userFirstNameLens) eric

-- updatedEricFirstName :: User
-- updatedEricFirstName =
--   (setter userFirstNameLens) eric "EEERRRRRIIIICCCCCC"



eric :: User
eric =
  User
    { glucoseLevel = 400
    , userName =
        Name
          { nameFirst = "Eric"
          , nameLast = "Jelliffe"
          }
    }

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

-- userGlucoseLens :: Lens User Int
userGlucoseLens
  :: forall f. Functor f
  => (Int -> f Int) -> User -> f User
userGlucoseLens f (User glucoseLevel userName) =
  fmap (\newGlucoseLevel -> User newGlucoseLevel userName) (f glucoseLevel)

data Const r a = Const r deriving Functor

unConst :: Const r a -> r
unConst (Const r) = r

instance Contravariant (Const r) where
  contramap :: (a -> b) -> Const r b -> Const r a
  contramap _ (Const r) = Const r

instance Monoid r => Applicative (Const r) where
  pure :: a -> Const r a
  pure _ = Const mempty

  (<*>) :: Const r (a -> b) -> Const r a -> Const r b
  Const r1 <*> Const r2 = Const (r1 <> r2)

-- class Invariant f where
--   imap :: (a -> a) -> f a -> f a

-- data MyInv a = MyInv (a -> a)

-- instance Invariant MyInv where
--   imap a2a (MyInv a2a') = MyInv $ \a -> a2a (a2a' (a2a a))

-- class (Functor f, Contravariant f) => Bivariant f

-- instance Functor (Const r) where
--   fmap :: (a -> b) -> Const r a -> Const r b
--   fmap _ (Const r) = Const r

-- view :: Lens s a -> s -> a
-- view :: (forall f. Functor f => (a -> f a) -> s -> f s) -> s -> a
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view somelens s = unConst (somelens Const s)

type Getting s a = (a -> Const a a) -> s -> Const a s

to :: (s -> a) -> ((a -> Const a a) -> s -> Const a s)
-- to :: (s -> a) -> Getting s a
to s2a a2Constaa s = fmap (\_ -> s) $ a2Constaa (s2a s)

type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

to' :: (s -> a) -> (forall f. (Functor f, Contravariant f) => (a -> f a) -> s -> f s)
-- to' :: (s -> a) -> Getter s a
to' s2a a2fa s = contramap s2a (a2fa (s2a s))

-- data Parser a = Parser (String -> a)

-- instance Functor Parser where
--   fmap :: (a -> b) -> Parser a -> Parser b
--   -- fmap :: (a -> b) -> (String -> a) -> (String -> b)
--   fmap a2b (Parser string2a) = Parser $ \string -> a2b (string2a string)

-- data Name = Name String

-- myFunc1 :: Parser String -> Parser Name
-- myFunc1 parserString = fmap Name parserString

-- data PrettyPrinter a = PrettyPrinter (a -> ByteString)

-- instance Contravariant PrettyPrinter where
--   contramap :: (a -> b) -> PrettyPrinter b -> PrettyPrinter a
--   -- contramap :: (a -> b) -> (b -> String) -> (a -> String)
--   contramap a2b (PrettyPrinter b2string) = PrettyPrinter $ \a -> b2string (a2b a)

-- myFunc2 :: PrettyPrinter Name -> PrettyPrinter String
-- myFunc2 prettyName = contramap (_ :: String -> Name) prettyName

viewM :: MonadReader s m => (forall f. Functor f => (a -> f a) -> s -> f s) -> m a
viewM somelens = do
  s <- ask
  pure $ unConst (somelens Const s)

myFooM :: (MonadReader User m, MonadIO m) => m Int
myFooM = do
  name <- viewM (userNameLens . nameFirstLens)
  pure (length name)

myFoo :: Int
myFoo =
  -- let ret = (viewM (userNameLens . nameFirstLens) :: MonadReader User m => m String) eric
  let ret = (viewM (userNameLens . nameFirstLens) :: User -> String) eric
  in length ret

data Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = Identity

  Identity f <*> Identity a = Identity (f a)

-- set :: Lens s a -> a -> s -> s
set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set somelens a s = runIdentity $ somelens (const $ Identity a) s

data User = User
  { glucoseLevel :: Int
  , userName :: Name
  } deriving Show

userNameLens
  :: forall f. Functor f
  => (Name -> f Name) -> User -> f User
  -- :: Lens User Name
userNameLens f (User glucoseLevel userName) =
  fmap (\newName -> User glucoseLevel newName) (f userName)

nameFirstLens
  :: forall f. Functor f => (String -> f String) -> Name -> f Name
  -- :: Lens Name String
nameFirstLens f (Name firstN lastN) =
  fmap (\newFirstName -> Name newFirstName lastN) (f firstN)


userFirstNameLens
  :: forall f. Functor f => (String -> f String) -> User -> f User
  -- :: Lens User String
userFirstNameLens = userNameLens . nameFirstLens

-- lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens :: (s -> a) -> (s -> a -> s) -> (forall f. Functor f => (a -> f a) -> s -> f s)
lens _ _ = undefined


-- type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s
type Traversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

data Name = Name
  { nameFirst :: String
  , nameLast :: String
  } deriving Show

namesTraversal
  :: forall f. Applicative f
  => (String -> f String) -> Name -> f Name
  -- :: Traversal Name String
namesTraversal str2fstr (Name nameFst nameLst) =
  Name <$> str2fstr nameFst <*> str2fstr nameLst

toListOf
  :: forall a s
   . ((a -> Const [a] a) -> s -> Const [a] s)
   -- . (Traversal s a)
  -> s
  -> [a]
toListOf trvsl s = unConst $ trvsl (\a -> Const [a]) s

-- > toListOf namesTraversal ericName :: [String]
-- > view namesTraversal eric -- error!
-- > set namesTraversal "dave" eric

ericName :: Name
ericName = Name "Eric" "Jelliffe"

-- Traversal [a] a

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

-- traverseList :: forall f. Applicative f => (a -> f a) -> [a] -> f [a]
-- traverseList :: Traversal [a] a


-- view     :: Lens      s a -> s -> a

userNamesTraversal :: Applicative f => (String -> f String) -> User -> f User
-- userNamesTraversal :: Traversal User String
userNamesTraversal = userNameLens . namesTraversal







-- forallExample1 :: forall a. (a -> a) -> Int
-- -- forallExample1 a2a = a2a @Int 3
-- forallExample1 _ = 4

-- callForallExample1 :: Int
-- callForallExample1 = forallExample1 @Int succ

-- forallExample2 :: (forall a. a -> a) -> Int
-- forallExample2 a2a = a2a @Int 3

-- callForallExample2 :: Int
-- -- callForallExample2 = forallExample2 (id @Int)
-- callForallExample2 = forallExample2 id


-- myid :: forall a. a -> a
-- myid a = a

-- myid :: forall a. a -> a
-- myidString :: String -> String

-- typeApplicationsExample :: Int
-- typeApplicationsExample =
--   -- (myid :: Int -> Int) (3 :: Int)
--   -- myid 3
--   (myid @String) 3


-- -- forallExample1
-- --   :: forall a. Show a => (a -> String) -> String
-- forallExample1
--   :: forall a. (a -> String) -> (a -> String) -> String
-- forallExample1 = undefined
-- -- forallExample1 a2a = a2a @Int 3
-- -- forallExample1 a2String = a2String show

-- -- forallExample1'
-- --   :: forall a. Show a => (String -> a) -> String
-- forallExample1'
--   :: forall a. (a -> String) -> (String -> a) -> String
-- forallExample1' = undefined

-- callForallExample1 :: String
-- callForallExample1 = forallExample1 @String succ

-- forallExample2
--   :: (forall a. Show a => a -> String) -> String
-- forallExample2 a2a = undefined -- a2a @Int 3

-- callForallExample2 :: String
-- -- callForallExample2 = forallExample2 (id @Int)
-- callForallExample2 = undefined -- forallExample2 id


-- myFunction :: forall a. Show a => a -> String
-- myFunction a = show a ++ show a
