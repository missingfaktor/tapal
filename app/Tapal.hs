module Tapal
  ( module Tapal
  ) where

import GHC.Generics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Catch
import Control.Exception
import System.IO.Error
import Control.Applicative
import Options.Applicative as O
import Data.Monoid
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Data.Yaml as Y
import Data.ByteString
import Data.Either.Combinators
import qualified Data.Map as Map
import Utilities
import Prelude hiding (readFile)

-- Data types

newtype Url = Url String
              deriving (Generic, Show)

data HttpMethod = Get | Post | Put | Patch | Delete | Connect
                  deriving (Generic, Show)

data Request = Request { url :: Url
                       , method :: HttpMethod
                       , headers :: Map.Map String String
                       } deriving (Generic, Show)

newtype TapalCommand = Issue { requestPath :: String
                             } deriving (Show)

-- JSON parsers (for YAML)

instance Y.FromJSON Url

instance Y.FromJSON HttpMethod where
  parseJSON (Y.String "GET")     = pure Get
  parseJSON (Y.String "POST")    = pure Post
  parseJSON (Y.String "PUT")     = pure Put
  parseJSON (Y.String "PATCH")   = pure Patch
  parseJSON (Y.String "DELETE")  = pure Delete
  parseJSON (Y.String "CONNECT") = pure Connect
  parseJSON badMethod            = fail ("Bad HTTP method: " ++ show badMethod)

instance FromJSON Request

-- Command line parsers

issueCommandParser :: O.Parser TapalCommand
issueCommandParser = Issue <$> O.argument O.str (O.metavar "REQUEST_PATH")

tapalCommandParser :: O.Parser TapalCommand
tapalCommandParser = O.hsubparser (O.command "issue" (O.info issueCommandParser (O.progDesc "Issue a request")))

tapalParserInfo :: O.ParserInfo TapalCommand
tapalParserInfo = O.info (tapalCommandParser <**> O.helper)
                         (O.fullDesc <> O.progDesc "Tapal, a lightweight command line alternative to Postman")

-- Execution

requestAtPath :: (MonadThrow m, MonadIO m) => FilePath -> m Request
requestAtPath path = do
  contents <- liftIO (readFile path)
  request <- raiseLeft (Y.decodeEither' @Request contents)
  return request

issueRequest :: MonadIO m => Request -> m ()
issueRequest request = liftIO (print request)

runTapal :: (MonadThrow m, MonadIO m) => m ()
runTapal = do
  tapalCommand <- liftIO (O.execParser tapalParserInfo)
  liftIO (print tapalCommand)
  case tapalCommand of
    Issue requestPath -> requestAtPath requestPath >>= issueRequest
