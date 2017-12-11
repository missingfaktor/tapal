module Utilities where

import Control.Monad.Catch
import Control.Exception
import Data.Either.Combinators

raiseLeft :: (Exception e, MonadThrow m) => Either e a -> m a
raiseLeft = either throwM pure
