module Probe
  ( Probe
  , probeAtDirectoryPath
  ) where

import Control.Monad
import System.IO.Error as E
import System.FilePath as F
import System.Directory as D

-- A probe is an instance of an "API probe", comprising the request to be made, scripts to run afterwards, variables
-- to set, and so on.
newtype Probe = Probe { directory :: String
                      } deriving (Show)

requireDirectoryExists :: FilePath -> IO ()
requireDirectoryExists directory = do
  itDoes <- D.doesDirectoryExist directory
  unless itDoes $
    E.ioError $ E.userError $ "Directory does not exist: " ++ directory

-- A probe will be mapped to a directory, with its individual components represented by files therein.
probeAtDirectoryPath :: FilePath -> IO Probe
probeAtDirectoryPath directory = do
  requireDirectoryExists directory
  return $ Probe directory
