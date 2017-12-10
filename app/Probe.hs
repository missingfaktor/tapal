{-# LANGUAGE FlexibleContexts #-}

module Probe
  ( Probe
  , probeAtDirectoryPath
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Exception
import System.IO.Error
import System.FilePath
import System.Directory

-- A probe is an instance of an "API probe", comprising the request to be made, scripts to run afterwards, variables
-- to set, and so on.
newtype Probe = Probe { directory :: String
                      } deriving (Show)

requireDirectoryExists :: (MonadError IOException m, MonadIO m) => FilePath -> m ()
requireDirectoryExists directory = do
  itDoes <- liftIO $ doesDirectoryExist directory
  unless itDoes $
    throwError $ userError $ "Directory does not exist: " ++ directory

-- A probe will be mapped to a directory, with its individual components represented by files therein.
probeAtDirectoryPath :: MonadIO m => FilePath -> m Probe
probeAtDirectoryPath directory = do
  liftIO $ requireDirectoryExists directory
  return $ Probe directory
