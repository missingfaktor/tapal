{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Applicative
import Options.Applicative as O
import Tapal as T
import Data.Monoid

data CliOptions = CliOptions { probe :: String
                             , scope :: Maybe String
                             } deriving (Show)

cliOptionsParser :: O.Parser CliOptions
cliOptionsParser =
  CliOptions <$> O.strOption (O.long "probe"
                           <> O.short 'p'
                           <> O.help "Path for the probe specification" )
             <*> optional (O.strOption (O.long "scope"
                                     <> O.short 's'
                                     <> O.help "Scope in which the request is to be made"))

cliOptionsParserInfo :: O.ParserInfo CliOptions
cliOptionsParserInfo =
  O.info (cliOptionsParser <**> O.helper)
         (O.fullDesc <> O.progDesc "Tapal, a lightweight command line alternative to Postman")

cliApp :: IO ()
cliApp = do
  cliOptions <- O.execParser cliOptionsParserInfo
  probe' <- T.probeAtDirectoryPath $ probe cliOptions
  print probe'
  print cliOptions

main :: IO ()
main = cliApp
