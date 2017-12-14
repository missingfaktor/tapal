module SyntaxHighlighting
  ( MonadHighlight(..)
  , Highlightable(..)
  ) where

import qualified Text.Highlighting.Pygments as P
import qualified Data.Aeson as J

class Monad m => MonadHighlight m where
   highlightString :: String -> String -> m String

instance MonadHighlight IO where
  highlightString lexerName text = do
    Just lexer <- P.getLexerByName lexerName
    P.highlight lexer P.terminalFormatter [("encoding", "utf-8"), ("style", "monokai")] text

class Highlightable a where
  highlight :: (MonadHighlight m) => a -> m String
  default highlight :: (MonadHighlight m, Show a) => a -> m String
  highlight = highlightString "haskell" . show

instance Highlightable J.Value where
  highlight = highlightString "json" . show
