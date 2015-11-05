{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick
  ( NewickForest 
  , NewickNode()
  , branchLength
  , descendants
  , isLeaf
  , newickLabel
  , newickNode
  , newickStreamParser
  ) where

import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Text.Megaparsec
import Text.Megaparsec.Prim        (MonadParsec)

newickStreamParser :: MonadParsec s m Char => m [NewickForest]
newickStreamParser = many (try newickForestDefinition <|> (pure <$> newickExtendedDefinition)) <* eof
