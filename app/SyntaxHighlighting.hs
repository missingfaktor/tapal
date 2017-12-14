module SyntaxHighlighting
  ( MonadHighlight(..)
  , Highlightable(..)
  , AnsiColor(..)
  , showInColor
  ) where

import qualified Text.Highlighting.Pygments as P
import qualified Data.Aeson as J

class Monad m => MonadHighlight m where
   highlightString :: String -> String -> m String

instance MonadHighlight IO where
  highlightString lexerName text = do
    Just lexer <- P.getLexerByName lexerName
    P.highlight lexer P.terminalFormatter [("encoding", "utf-8")] text

class Highlightable a where
  highlight :: (MonadHighlight m) => a -> m String
  default highlight :: (MonadHighlight m, Show a) => a -> m String
  highlight = highlightString "haskell" . show

instance Highlightable J.Value where
  highlight = highlightString "json" . show

data AnsiColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White

ansiEscapeCode :: AnsiColor -> String
ansiEscapeCode Black   = "\27[30m"
ansiEscapeCode Red     = "\27[31m"
ansiEscapeCode Green   = "\27[32m"
ansiEscapeCode Yellow  = "\27[33m"
ansiEscapeCode Blue    = "\27[34m"
ansiEscapeCode Magenta = "\27[35m"
ansiEscapeCode Cyan    = "\27[36m"
ansiEscapeCode White   = "\27[37m"

ansiReset :: String
ansiReset = "\27[0m"

ansiColoredString :: AnsiColor -> String -> String
ansiColoredString color string = ansiEscapeCode color ++ string ++ ansiReset

showInColor :: Show a => AnsiColor -> a -> String
showInColor color = ansiColoredString color . show
