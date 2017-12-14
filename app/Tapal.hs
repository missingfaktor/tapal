module Tapal
  ( module Tapal
  ) where

import GHC.Generics
import Control.Monad
import Control.Monad.Catch
import Control.Applicative
import Data.Monoid
import Control.Monad.IO.Class

import System.FilePath (FilePath)
import Data.Function ((&))
import Utilities (raiseLeft)
import Data.Coerce (coerce)

import Prelude hiding (readFile)

import qualified Options.Applicative as O
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.Char8 as B8
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified SyntaxHighlighting as SH

-- Data types

-- These newtypes were written so we can write type-class instances for the underlying types.
-- RString simply means Strings used in Request fields such as method and headers. I could not think of a better name.
newtype RString = RString B8.ByteString deriving (Show)
newtype CaseInsensitiveRString = CaseInsensitiveRString (CI.CI BS.ByteString) deriving (Show, Eq, Ord)

data Request = Request { url :: String
                       , method :: RString
                       , headers :: Map.Map CaseInsensitiveRString RString
                       } deriving (Generic, Show)

newtype Response a = Response (N.Response a) deriving (Show)

newtype TapalCommand = Issue { requestPath :: String
                             } deriving (Show)

-- JSON parsers (for YAML)

instance A.FromJSON RString where
  parseJSON (A.String text) = pure (coerce (TE.encodeUtf8 text))
  parseJSON _ = fail "Could not decode as ByteString"

instance A.FromJSONKey CaseInsensitiveRString where
  fromJSONKey = A.FromJSONKeyText (coerce . CI.mk . TE.encodeUtf8)
  fromJSONKeyList = A.FromJSONKeyText ((:[]) . coerce . CI.mk . TE.encodeUtf8)

instance A.FromJSON Request

-- Highlightable instances

instance SH.Highlightable Request
instance Show a => SH.Highlightable (Response a)

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
  contents <- liftIO (BS.readFile path)
  raiseLeft (Y.decodeEither' contents)

issueRequest :: (MonadThrow m, MonadIO m) => Request -> m (Response LBS.ByteString)
issueRequest (Request url' (RString method') headers') = do
  parsedRequest <- N.parseRequest url'
  let amendedRequest = parsedRequest &
                       N.setRequestMethod method' &
                       N.setRequestHeaders (coerce (Map.toList headers'))
  response <- N.httpLBS amendedRequest
  return (Response response)

runTapal :: (MonadThrow m, SH.MonadHighlight m, MonadIO m) => m ()
runTapal = do
  tapalCommand <- liftIO (O.execParser tapalParserInfo)
  liftIO (print tapalCommand)
  case tapalCommand of
    Issue requestPath' -> do
      request <- requestAtPath requestPath'
      highlightedRequest <- SH.highlight request
      liftIO (putStrLn "Issuing a request:")
      liftIO (putStrLn highlightedRequest)
      response <- issueRequest request
      highlightedResponse <- SH.highlight response
      liftIO (putStrLn "Response:")
      liftIO (putStrLn highlightedResponse)
