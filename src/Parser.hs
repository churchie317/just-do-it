module Parser 
    ( run
    ) where

import Data.Semigroup ((<>))
import Task
import Options.Applicative 

data Input
    = Message String
    | Report

-- TODO: env flag for testing

run :: IO ()
run = execParser opts >>= program 

opts :: ParserInfo Input
opts = info (input <**> helper)
    ( fullDesc
    <> progDesc "doing - Save yourself from later."
    <> header "Reminding you of what's important since 2018." )

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

program :: Input -> IO ()
program (Message text) = createTask text 
program Report = showTasks 