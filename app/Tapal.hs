module Tapal
  ( module Tapal
  ) where

import Control.Applicative
import Options.Applicative as O
import Data.Monoid
import Control.Monad.IO.Class

newtype TapalCommand = Issue { requestPath :: String
                             } deriving (Show)

issueParser :: O.Parser TapalCommand
issueParser = Issue <$> O.argument O.str (O.metavar "REQUEST_PATH")

tapalCommandParser :: O.Parser TapalCommand
tapalCommandParser = O.hsubparser (O.command "issue" (O.info issueParser (O.progDesc "Issue a request")))

tapalParserInfo :: O.ParserInfo TapalCommand
tapalParserInfo = O.info (tapalCommandParser <**> O.helper)
                         (O.fullDesc <> O.progDesc "Tapal, a lightweight command line alternative to Postman")

runTapal :: MonadIO m => m ()
runTapal = do
  tapalCommand <- liftIO (O.execParser tapalParserInfo)
  liftIO (print tapalCommand)
