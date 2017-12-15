module Tapal.Utilities
  ( raiseLeft
  ) where

import Control.Monad.Catch

raiseLeft :: (Exception e, MonadThrow m) => Either e a -> m a
raiseLeft = either throwM pure
