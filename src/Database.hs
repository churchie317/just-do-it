module Database
    ( readTasks
    , writeTasks
    ) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Maybe (catMaybes, fromMaybe)
import Data.Task 
import System.Directory (doesFileExist, getHomeDirectory)

filename :: String
filename = "/.doing_data"

-- Reads list of tasks from disk
readTasks :: IO [Task]
readTasks = do
    confirmOrCreateDatabase
    path <- Database.filepath
    maybeTasks <- decodeStrict <$> B.readFile path
    return $ fromMaybe [] maybeTasks

-- Serializes list of tasks to disk
writeTasks :: [Task] -> IO ()
writeTasks tasks = do
    path <- Database.filepath
    L.writeFile path $ encode tasks

confirmOrCreateDatabase :: IO ()
confirmOrCreateDatabase = do
    exists <- doesDatabaseExist
    unless exists <$> createFrom =<< filepath 

filepath :: IO FilePath
filepath = flip (++) filename <$> getHomeDirectory

doesDatabaseExist :: IO Bool
doesDatabaseExist = doesFileExist =<< filepath

createFrom :: FilePath -> IO ()
createFrom path = writeFile path ""