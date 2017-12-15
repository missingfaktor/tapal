module Tapal
  ( module Tapal
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import System.FilePath (FilePath)
import Data.Function ((&))

import Prelude hiding (readFile)

import qualified Network.HTTP.Simple as N
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Tapal.Types as T
import qualified Tapal.SyntaxHighlighting as SH
import qualified Tapal.CommandLine as C
import qualified Tapal.Utilities as U

requestAtPath :: (MonadThrow m, MonadIO m) => FilePath -> m T.Request
requestAtPath path = do
  contents <- liftIO (BS.readFile path)
  U.raiseLeft (Y.decodeEither' contents)

makeRequest :: (MonadThrow m, MonadIO m) => T.Request -> m (T.Response LBS.ByteString)
makeRequest (T.Request (T.Url url) (T.HttpMethod method) (T.Headers headers)) = do
  let headers' = map (\case (T.Header key value) -> (key, value)) headers
  parsedRequest <- N.parseRequest url
  let amendedRequest = parsedRequest &
                       N.setRequestMethod method &
                       N.setRequestHeaders headers'
  response <- N.httpLBS amendedRequest
  return (T.Response response)

runTapal :: (MonadThrow m, SH.MonadHighlight m, MonadIO m) => m ()
runTapal = do
  tapalCommand <- C.parseTapalCommand
  liftIO (print tapalCommand)
  case tapalCommand of
    C.RequestCommand requestPath' -> do
      request <- requestAtPath requestPath'
      highlightedRequest <- SH.highlight request
      liftIO (putStrLn "Making a request:")
      liftIO (putStrLn highlightedRequest)
      response <- makeRequest request
      highlightedResponse <- SH.highlight response
      liftIO (putStrLn "Response:")
      liftIO (putStrLn highlightedResponse)

main :: IO ()
main = runTapal
