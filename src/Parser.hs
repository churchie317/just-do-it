module Parser 
    ( run
    ) where

import Data.Semigroup ((<>))
import Task (createTask, showNTasks)
import Options.Applicative 
import Options.Applicative.Help (bold)

data Input
    = Message String
    | Report Int

-- TODO: env flag for testing

run :: IO ()
run = execParserWithPrefs opts >>= program 

execParserWithPrefs :: ParserInfo Input -> IO Input
execParserWithPrefs = customExecParser $ prefs showHelpOnEmpty

opts :: ParserInfo Input
opts = info (input <**> helper)
    ( fullDesc
    <> header "doing - Save yourself from later"
    <> progDesc "A snippets tracker for ideas, tasks, and TODOs" )

input :: Parser Input
input = messageInput <|> reportInput

messageInput :: Parser Input
messageInput = Message <$> strOption
    ( long "message"
    <> short 'm'
    <> metavar "TEXT"
    <> style bold
    <> help "Short message describing activity" )

reportInput :: Parser Input
reportInput = Report <$> option auto
    ( long "report-last"
    <> short 'r'
    <> metavar "INT"
    <> style bold
    <> help "Number of days of snippets to show from current date" )

program :: Input -> IO ()
program (Message text) = createTask text 
program (Report n) = showNTasks n