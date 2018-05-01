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

-- The file name where application data will be written
filename :: String
filename = "/.doing_data"

-- Reads List of Task from disk
readTasks :: IO [Task]
readTasks = do
    confirmOrCreateDatabase
    path <- Database.filepath
    maybeTasks <- decodeStrict <$> B.readFile path
    return $ fromMaybe [] maybeTasks

-- Serializes List of Task to disk
writeTasks :: [Task] -> IO ()
writeTasks tasks = do
    path <- Database.filepath
    L.writeFile path $ encode tasks

-- Confirms existence of data store: returns if found; 
-- otherwise creates store
confirmOrCreateDatabase :: IO ()
confirmOrCreateDatabase = do
    exists <- doesDatabaseExist
    unless exists <$> createFrom =<< filepath 

-- Concatenates file name to Home directory
filepath :: IO FilePath
filepath = (++filename)  <$> getHomeDirectory

doesDatabaseExist :: IO Bool
doesDatabaseExist = doesFileExist =<< filepath

createFrom :: FilePath -> IO ()
createFrom path = writeFile path ""