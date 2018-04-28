{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.List (groupBy, intercalate, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup ((<>))
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import Options.Applicative 
-- import Parser
import System.Directory (doesFileExist, getHomeDirectory)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper)
      (fullDesc
     <> progDesc "doing - Save yourself from later."
     <> header "doing" )

run :: Input -> IO ()
run (Message text) = createTask text 
run (Report) = do
  tasks <- readTasks  
  putStrLn (renderTaskGroups (groupTasks tasks))

data Input
  = Message String
  | Report

input :: Parser Input
input = messageInput <|> reportInput

messageInput :: Parser Input
messageInput = Message <$> strOption
  ( long "message"
  <> short 'm'
  <> metavar "TEXT"
  <> help "Short message describing activity")

reportInput :: Parser Input
reportInput = flag' Report
    ( long "report"
    <> short 'r'
    <> help "Number of days to show" )


data TaskGroup = TaskGroup {
  groupTime :: UTCTime,
  tasks :: [Task]
} deriving (Show)

data Task = Task {
  text :: String,
  time :: UTCTime
} deriving (Show, Generic)

instance ToJSON Task
instance FromJSON Task

databaseFilePath :: IO FilePath
databaseFilePath = do
  home <- getHomeDirectory
  return $ home ++ "/.doing_data"

createDatabase :: IO ()
createDatabase = do
  path <- databaseFilePath
  writeFile path ""

databaseExists :: IO Bool
databaseExists =
  databaseFilePath >>= doesFileExist

confirmDatabase :: IO ()
confirmDatabase = do
  exists <- databaseExists
  when (not exists) createDatabase

-- Reads the tasks from disk
readTasks :: IO [Task]
readTasks = do
  confirmDatabase
  path <- databaseFilePath
  maybeTasks <- fmap decodeStrict $ B.readFile path
  return $ fromMaybe [] maybeTasks

-- Writes the tasks to disk
saveTasks :: [Task] -> IO ()
saveTasks tasks = do
  path <- databaseFilePath
  L.writeFile path $ encode tasks

mkTask :: String -> IO Task 
mkTask text = do
  utcTime <- getCurrentTime
  return $ Task text utcTime

-- Formats a list of tasks into a string
showTasks :: [Task] -> String
showTasks tasks =
  undefined

mkTaskGroup :: [Task] -> Maybe TaskGroup
mkTaskGroup tasks@((Task _ time):_) = Just $ TaskGroup time tasks
mkTaskGroup _ = Nothing


groupTasks :: [Task] -> [TaskGroup]
groupTasks tasks =
  catMaybes $ map mkTaskGroup groupedTasks 
  where
     groupTasks (Task _ t1) (Task _ t2) = utctDay t1 == utctDay t2
     groupedTasks = groupBy groupTasks $ sortOn time tasks

renderTaskGroups :: [TaskGroup] -> String
renderTaskGroups = intercalate "\n" . map renderTaskGroup

renderTaskGroup :: TaskGroup -> String
renderTaskGroup (TaskGroup time tasks) =
  intercalate "\n" $ formatTime defaultTimeLocale "%A, %B %e" time
  : map renderTask tasks

renderTask :: Task -> String
renderTask (Task text _) = "- " ++ text

createTask :: String -> IO ()
createTask text = do
  tasks <- readTasks
  task <- mkTask text
  saveTasks $ task : tasks

-- Tuesday, January 28th
-- - Wrote a haskell app
-- - Refactored Accountify again
-- 
-- Wednesday, January 29th
-- - Used the bathroom 5 times
-- - Refactored Accountify again