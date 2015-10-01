{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick
  ( NewickForest 
  , NewickNode()
  , branchLength
  , descendants
  , isLeaf
  , newickLabel
  , newickNode
  , parseNewickStream
  ) where

import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Text.Parsec

parseNewickStream :: Stream s m Char => s -> m (Either ParseError [NewickForest])
parseNewickStream = runParserT (many (try newickForestDefinition <|> (pure <$> newickExtendedDefinition)) <* eof) () "One or more Extended Newick Forest definitions"