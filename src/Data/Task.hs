{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Task 
    ( Task (..)
    , TaskGroup (..)
    ) where

import Data.Aeson
import Data.Time.Clock
import GHC.Generics

data Task = Task {
    text :: String,
    time :: UTCTime
} deriving (Show, Generic)

data TaskGroup = TaskGroup {
    groupTime :: UTCTime,
    tasks :: [Task]
} deriving (Show)

instance ToJSON Task
instance FromJSON Task