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
import Data.CaseInsensitive as CI
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Data.Yaml as Y
import Data.Aeson.Types as A
import Data.ByteString
import Data.Either.Combinators
import qualified Data.Map as Map
import Utilities
import qualified Network.HTTP.Simple as N
import qualified Network.HTTP.Types.Header as N
import Data.Function
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Prelude hiding (readFile)

-- Data types

data Request = Request { url :: String
                       , method :: B8.ByteString
                       , headers :: Map.Map N.HeaderName B8.ByteString
                       } deriving (Generic, Show)

newtype TapalCommand = Issue { requestPath :: String
                             } deriving (Show)

-- JSON parsers (for YAML)

instance FromJSON B8.ByteString where
  parseJSON (Y.String text) =
    text &
      Data.Text.unpack &
      B8.pack &
      pure

  parseJSON value = fail "Could not decode as ByteString"

instance A.FromJSONKey N.HeaderName where
  fromJSONKey = FromJSONKeyText (CI.mk . B8.pack . Data.Text.unpack)
  fromJSONKeyList = FromJSONKeyText ((:[]) . CI.mk . B8.pack . Data.Text.unpack)

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

issueRequest :: (MonadThrow m, MonadIO m) => Request -> m ()
issueRequest (Request url method headers) = do
  parsedRequest <- N.parseRequest url
  let amendedRequest = parsedRequest &
                       N.setRequestMethod method &
                       N.setRequestHeaders (Map.toList headers)
  liftIO (print amendedRequest)

runTapal :: (MonadThrow m, MonadIO m) => m ()
runTapal = do
  tapalCommand <- liftIO (O.execParser tapalParserInfo)
  liftIO (print tapalCommand)
  case tapalCommand of
    Issue requestPath -> requestAtPath requestPath >>= issueRequest
