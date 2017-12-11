{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Options.Applicative as O
import Tapal as T

cliApp :: IO ()
cliApp = do
  cliOptions <- O.execParser T.cliOptionsParserInfo
  print cliOptions

main :: IO ()
main = cliApp
