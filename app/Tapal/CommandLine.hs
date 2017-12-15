module Tapal.CommandLine
  ( TapalCommand(..)
  , parseTapalCommand
  ) where

import qualified Options.Applicative as O

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<**>))
import Data.Monoid ((<>))

newtype TapalCommand = RequestCommand { requestPath :: String
                                      } deriving (Show)

requestCommandParser :: O.Parser TapalCommand
requestCommandParser = RequestCommand <$> O.argument O.str (O.metavar "REQUEST_PATH")

tapalCommandParser :: O.Parser TapalCommand
tapalCommandParser = O.hsubparser (O.command "request" (O.info requestCommandParser (O.progDesc "Make a request")))

tapalParserInfo :: O.ParserInfo TapalCommand
tapalParserInfo = O.info (tapalCommandParser <**> O.helper)
                         (O.fullDesc <> O.progDesc "Tapal, a lightweight command line alternative to Postman")

parseTapalCommand :: MonadIO m => m TapalCommand
parseTapalCommand = liftIO (O.execParser tapalParserInfo)
