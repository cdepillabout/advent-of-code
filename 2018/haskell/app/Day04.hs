module Main where

import Control.Lens
import Control.Monad (void)
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Function (on)
import qualified Data.List as List
import Data.Map (Map, insertWith, toList)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Text.Parsec as Parsec
import Text.Pretty.Simple

import Debug.Trace

-- $setup
--
-- >>> exampleTime = maybe (error "NOT A TIME") id $ parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M" "2000-01-02 03:04"

main :: IO ()
main =
  Text.interact mySolutionPart1

type GuardId = Int

data Record = BeginShift GuardId | WakeUp | FallAsleep deriving Show

data GuardRecord = GuardRecord
  { guardRecordTime :: UTCTime
  , guardRecordRecord :: Record
  } deriving Show

data GuardSleepTime = GuardSleepTime
  { guardSleepTimeId :: GuardId
  , guardSleepTimeTimes :: [(UTCTime, UTCTime)]
  } deriving Show

type Parser = Parsec Void Text

-- |
--
-- >>> -- parseTest parseClaims "#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
parseGuardRecords :: Parser [GuardRecord]
parseGuardRecords = do
  res <- many $ do
    guardRecord <- parseGuardRecord
    void $ optional eol
    pure guardRecord
  eof
  pure res


parseRecord :: Parser Record
parseRecord =
  let fallsAsleep = string "falls asleep" $> FallAsleep
      wakesUp = string "wakes up" $> WakeUp
      guardNum = do
        void $ string "Guard #"
        n <- decimal
        void $ string " begins shift"
        pure $ BeginShift n
  in fallsAsleep <|> wakesUp <|> guardNum

-- |
--
-- >>> parseTest parseGuardRecord "[1518-11-01 00:05] falls asleep"
-- GuardRecord {guardRecordTime = 2000-11-01 00:05:00 UTC, guardRecordRecord = FallAsleep}
--
-- >>> parseTest parseGuardRecord "[1518-11-01 00:25] wakes up"
-- GuardRecord {guardRecordTime = 2000-11-01 00:25:00 UTC, guardRecordRecord = WakeUp}
--
-- >>> parseTest parseGuardRecord "[1518-11-01 00:00] Guard #10 begins shift"
-- GuardRecord {guardRecordTime = 2000-11-01 00:00:00 UTC, guardRecordRecord = BeginShift 10}
parseGuardRecord :: Parser GuardRecord
parseGuardRecord = do
  void $ char '['
  rawTime <- takeWhile1P Nothing (/= ']')
  let rawTime' = "2000" <> Text.drop 4 rawTime
  time <- parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M" (Text.unpack rawTime')
  void $ string "] "
  rec <- parseRecord
  pure $ GuardRecord time rec

-- parseSingleDays :: [GuardRecord] -> Seq GuardSleepTime
-- parseSingleDays = go empty
--   where
--     go :: Seq GuardSleepTime -> [GuardRecord] -> Seq GuardSleepTime
--     go jk(GuardRecord time (BeginShift guardId) : grs) =


daysParser :: Parsec.Parsec [GuardRecord] () [GuardSleepTime]
daysParser = do
  res <- Parsec.many1 dayParser
  Parsec.eof
  pure res

dayParser :: Parsec.Parsec [GuardRecord] () GuardSleepTime
dayParser = do
  guardId <- beginShiftParser
  startEndSleep <- Parsec.many sleepChangeParser
  pure $ GuardSleepTime guardId startEndSleep

-- |
--
-- >>> Parsec.parseTest beginShiftParser [GuardRecord exampleTime (BeginShift 100)]
-- 100
beginShiftParser :: Parsec.Parsec [GuardRecord] () GuardId
beginShiftParser = do
  Parsec.tokenPrim show (\sp _ _ -> sp) match'
  where
    match' :: GuardRecord -> Maybe GuardId
    match' (GuardRecord _ (BeginShift gid)) = Just gid
    match' _ = Nothing

-- |
--
-- >>> Parsec.parseTest fallAsleepParser [GuardRecord exampleTime FallAsleep]
-- 2000-01-02 03:04:00 UTC
fallAsleepParser :: Parsec.Parsec [GuardRecord] () UTCTime
fallAsleepParser = do
  Parsec.tokenPrim show (\sp _ _ -> sp) match'
  where
    match' :: GuardRecord -> Maybe UTCTime
    match' (GuardRecord time FallAsleep) = Just time
    match' _ = Nothing

-- |
--
-- >>> Parsec.parseTest wakeUpParser [GuardRecord exampleTime WakeUp]
-- 2000-01-02 03:04:00 UTC
wakeUpParser :: Parsec.Parsec [GuardRecord] () UTCTime
wakeUpParser = do
  Parsec.tokenPrim show (\sp _ _ -> sp) match'
  where
    match' :: GuardRecord -> Maybe UTCTime
    match' (GuardRecord time WakeUp) = Just time
    match' _ = Nothing

-- |
--
-- >>> Parsec.parseTest sleepChangeParser [GuardRecord exampleTime FallAsleep, GuardRecord exampleTime WakeUp]
-- (2000-01-02 03:04:00 UTC,2000-01-02 03:04:00 UTC)
sleepChangeParser :: Parsec.Parsec [GuardRecord] () (UTCTime, UTCTime)
sleepChangeParser = do
  (,) <$> fallAsleepParser <*> wakeUpParser

-- | Take a list of records per night per guard, and return all the times they
-- have been asleep.
getRecsPerGuard :: [GuardSleepTime] -> Map GuardId [(UTCTime, UTCTime)]
getRecsPerGuard gsts =
  unionsWith (<>) $ fmap (\(GuardSleepTime i ts) -> singleton i ts) gsts

mySolutionPart1 :: Text -> Text
mySolutionPart1 input =
  let guardRecords =
        fromMaybe
          (error "could not parse the input")
          (parseMaybe parseGuardRecords input)
      sortedGuardRecords = List.sortBy (compare `on` guardRecordTime) guardRecords
      (allSingleDaysRecords :: [GuardSleepTime]) =
        either
          (\err -> error $ "could not parse the sorted recs: " ++ show err)
          id
          (Parsec.parse daysParser "" sortedGuardRecords)
      (recordsPerGuard :: Map GuardId [(UTCTime, UTCTime)]) = undefined
  -- in LText.toStrict $ pShow (List.take 10 guardRecords)
  -- in LText.toStrict $ pShow (List.take 10 sortedGuardRecords)
  in LText.toStrict $ pShow (List.take 10 allSingleDaysRecords)
