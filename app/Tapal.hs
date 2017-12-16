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

import qualified Tapal.Types as T
import qualified Tapal.SyntaxHighlighting as SH
import qualified Tapal.CommandLine as C
import qualified Tapal.Utilities as U

requestAtPath :: (MonadThrow m, MonadIO m) => FilePath -> m T.Request
requestAtPath path = do
  contents <- liftIO (BS.readFile path)
  U.raiseLeft (Y.decodeEither' contents)

makeRequest :: (MonadThrow m, MonadIO m) => T.Request -> m T.Response
makeRequest (T.Request url method headers) = do
  parsedRequest <- N.parseRequest (T.unrefashionUrl url)
  let amendedRequest = parsedRequest &
                       N.setRequestMethod (T.unrefashionMethod method) &
                       N.setRequestHeaders (map T.unrefashionHeader (T.getHeaders headers))
  response <- N.httpLBS amendedRequest
  return (T.refashionResponse response)

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
