module Parser 
    ( Input(..)
    , input
    ) where

import Data.Semigroup ((<>))
import Options.Applicative 

data Input
  = Message String
  | Report

-- TODO: env flag for testing

input :: Parser Input
input = messageInput <|> reportInput

messageInput :: Parser Input
messageInput = Message <$> strOption
  ( long "message"
  <> short 'm'
  <> metavar "TEXT"
  <> help "Short message describing activity" )

reportInput :: Parser Input
reportInput = flag' Report
    ( long "report"
    <> short 'r'
    <> help "Number of days to show" )