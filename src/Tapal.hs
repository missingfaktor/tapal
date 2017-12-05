{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tapal
    ( someFunc
    ) where

import Options.Generic as O

data CliOptions = CliOptions { request :: String O.<?> "Path for the request specification"
                             , scope :: Maybe String O.<?> "Scope in which the request is to be made"
                             } deriving (Generic, Show)

cliOptionModifiers :: O.Modifiers
cliOptionModifiers = O.defaultModifiers { shortNameModifier = O.firstLetter }

instance O.ParseRecord CliOptions where
  parseRecord = parseRecordWithModifiers cliOptionModifiers

someFunc :: IO ()
someFunc = do
  cliOptions <- O.getRecord @_ @CliOptions "Tapal, a lightweight command line alternative to Postman"
  print cliOptions
