{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Options.Generic as O
import Tapal as T

data CliOptions = CliOptions { probe :: String O.<?> "Path for the probe specification"
                             , scope :: Maybe String O.<?> "Scope in which the request is to be made"
                             } deriving (Generic, Show)

cliOptionModifiers :: O.Modifiers
cliOptionModifiers = O.defaultModifiers { shortNameModifier = O.firstLetter }

instance O.ParseRecord CliOptions where
  parseRecord = parseRecordWithModifiers cliOptionModifiers

cliApp :: IO ()
cliApp = do
  cliOptions <- O.getRecord @_ @CliOptions "Tapal, a lightweight command line alternative to Postman"
  probe' <- T.probeAtDirectoryPath $ unHelpful $ probe $ cliOptions
  print probe'
  print cliOptions

main :: IO ()
main = cliApp
