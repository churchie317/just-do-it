{-# LANGUAGE NamedFieldPuns #-}

module Task 
    ( create
    , show
    ) where

import qualified Database
import Data.List (groupBy, intercalate, sortOn)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Task
import Data.Time.Clock 
import Data.Time.Format (defaultTimeLocale, formatTime)
import Prelude hiding (show)

-- Date printing format
timeStringFormat :: String
timeStringFormat = "%A, %B %e"

-- Prepends Task to List of Task and serializes to disk
create :: String -> IO ()
create text = do
    tasks <- Database.readTasks
    task <- mkTask text
    Database.writeTasks $ task : tasks

-- Reads List of Task from disk and pretty prints them to console
show :: IO ()
show = putStrLn =<< renderTaskGroups . groupTasks <$> Database.readTasks

-- Groups List of Task into List of TaskGroup by date
groupTasks :: [Task] -> [TaskGroup]
groupTasks tasks =
    mapMaybe mkTaskGroup groupedTasks
    where
        groupedTasks = groupBy isSameDay $ sortOn time tasks

-- Compares two tasks: returns True if same day; otherwise False
isSameDay :: Task -> Task -> Bool
isSameDay Task{time=t1} Task{time=t2} = utctDay t1 == utctDay t2

mkTask :: String -> IO Task 
mkTask text = Task text <$> getCurrentTime

mkTaskGroup :: [Task] -> Maybe TaskGroup
mkTaskGroup tasks@(Task{time}:_) = Just $ TaskGroup time tasks
mkTaskGroup _ = Nothing

renderTaskGroups :: [TaskGroup] -> String
renderTaskGroups = intercalate "\n" . map renderTaskGroup

renderTaskGroup :: TaskGroup -> String
renderTaskGroup TaskGroup{groupTime, tasks} =
    intercalate "\n" $ formattedTime groupTime : map renderTask tasks

renderTask :: Task -> String
renderTask Task{text} = "- " ++ text

-- Formatted date string
formattedTime :: UTCTime -> String
formattedTime = formatTime defaultTimeLocale timeStringFormat