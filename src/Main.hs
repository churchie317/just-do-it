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
import Parser
import System.Directory (doesFileExist, getHomeDirectory)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper)
      (fullDesc
     <> progDesc "doing - Save yourself from later."
     <> header "Reminding you of what's important since 2018." )

run :: Input -> IO ()
run (Message text) = createTask text 
run (Report) = do
  tasks <- readTasks  
  putStrLn . renderTaskGroups $ groupTasks tasks

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

-- Prepends Task to List of Tasks and serializes ot disk
createTask :: String -> IO ()
createTask text = do
  tasks <- readTasks
  task <- mkTask text
  saveTasks $ task : tasks

-- Make Task from text
mkTask :: String -> IO Task 
mkTask text = do
  utcTime <- getCurrentTime
  return $ Task text utcTime

-- Serializes list of tasks to disk
saveTasks :: [Task] -> IO ()
saveTasks tasks = do
  path <- databaseFilePath
  L.writeFile path $ encode tasks

-- Reads list of tasks from disk
readTasks :: IO [Task]
readTasks = do
  confirmDatabase
  path <- databaseFilePath
  maybeTasks <- fmap decodeStrict $ B.readFile path
  return $ fromMaybe [] maybeTasks

renderTaskGroups :: [TaskGroup] -> String
renderTaskGroups = intercalate "\n" . map renderTaskGroup

confirmDatabase :: IO ()
confirmDatabase = do
  exists <- databaseExists
  when (not exists) createDatabase

databaseFilePath :: IO FilePath
databaseFilePath = do
  home <- getHomeDirectory
  return $ home ++ "/.doing_data"

databaseExists :: IO Bool
databaseExists =
  databaseFilePath >>= doesFileExist

createDatabase :: IO ()
createDatabase = do
  path <- databaseFilePath
  writeFile path ""

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

renderTaskGroup :: TaskGroup -> String
renderTaskGroup (TaskGroup time tasks) =
  intercalate "\n" $ formatTime defaultTimeLocale "%A, %B %e" time
  : map renderTask tasks

renderTask :: Task -> String
renderTask (Task text _) = "- " ++ text