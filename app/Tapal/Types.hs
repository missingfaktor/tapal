module Tapal.Types
  ( Url(..)
  , HttpMethod(..)
  , Header(..)
  , Headers(..)
  , Request(..)
  , Response(..)
  , refashionResponseStatus
  , refashionResponse
  , unrefashionUrl
  , unrefashionMethod
  , unrefashionHeader
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as BS
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Encoding as TE
import qualified Tapal.SyntaxHighlighting as SH
import qualified Network.HTTP.Simple as N
import qualified Network.HTTP.Types as N

import GHC.Generics (Generic)
import Data.Coerce (coerce)

newtype Url = Url
  { getUrl :: String
  } deriving (Show)

newtype HttpMethod = HttpMethod
  { getHttpMethod :: BS.ByteString
  } deriving (Show)

data Header = Header
  { headerKey :: CI.CI BS.ByteString
  , headerValue :: BS.ByteString
  } deriving (Show)

newtype Headers = Headers
  { getHeaders :: [Header]
  } deriving (Show)

data Request = Request
  { url :: Url
  , method :: HttpMethod
  , headers :: Headers
  } deriving (Generic, Show)

data ResponseStatus = ResponseStatus
  { statusCode :: Int
  , statusMessage :: BS.ByteString
  } deriving (Show)

newtype Response = Response
  { responseStatus :: ResponseStatus
  } deriving (Show)

-- "Refashioning" from Network.HTTP types to our types

refashionResponseStatus :: N.Status -> ResponseStatus
refashionResponseStatus nStatus = ResponseStatus (N.statusCode nStatus) (N.statusMessage nStatus)

refashionResponse :: N.Response a -> Response
refashionResponse nResponse = Response (refashionResponseStatus (N.getResponseStatus nResponse))

unrefashionUrl :: Url -> String
unrefashionUrl = getUrl

unrefashionMethod :: HttpMethod -> BS.ByteString
unrefashionMethod = getHttpMethod

unrefashionHeader :: Header -> N.Header
unrefashionHeader (Header key value) = (key, value)

-- JSON parsers (for YAML)

deriving instance J.FromJSON Url

instance J.FromJSON HttpMethod where
  parseJSON (J.String text) = pure (coerce (TE.encodeUtf8 text))
  parseJSON other           = fail ("Could not decode HttpMethod: " ++ show other)

instance J.FromJSON Headers where
  parseJSON (J.Object object) =
    Headers <$> traverse mapToHeader (HM.toList object)
    where
      mapToHeader (key, J.String value) = pure (Header (CI.mk (TE.encodeUtf8 key)) (TE.encodeUtf8 value))
      mapToHeader (  _, other)          = fail ("Could not decode header value: " ++ show other)

  parseJSON other             = fail ("Could not decode Headers: " ++ show other)

instance J.FromJSON Request

-- Syntax highlighting

instance SH.Highlightable Request

instance SH.Highlightable ResponseStatus where
  highlight status = do
    let statusCode' = SH.showInColor SH.Yellow (statusCode status)
    let statusMessage' = SH.showInColor SH.Green (statusMessage status)
    return (statusCode' ++ " " ++ statusMessage')

instance SH.Highlightable Response where
  highlight response = SH.highlight (responseStatus response)
