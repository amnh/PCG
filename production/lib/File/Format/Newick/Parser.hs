{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick.Parser where

import Control.Monad              (liftM)
import Data.Char                  (isSpace)
import Data.List                  (intercalate)
import Data.Map            hiding (filter,foldl,foldr,null)
import Data.Maybe                 (fromJust,fromMaybe,isJust)
import File.Format.Newick.Internal
import Prelude             hiding (lookup)
import Text.Megaparsec     hiding (label)
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim       (MonadParsec)

newickStandardDefinition :: MonadParsec s m Char => m NewickNode
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')

newickExtendedDefinition :: MonadParsec s m Char => m NewickNode
newickExtendedDefinition = newickStandardDefinition >>= joinNonUniqueLabeledNodes

newickForestDefinition :: MonadParsec s m Char => m NewickForest
newickForestDefinition = whitespace *> symbol (char '<') *> many newickExtendedDefinition <* symbol (char '>')

newickNodeDefinition :: MonadParsec s m Char => m NewickNode
newickNodeDefinition = do
    descendants'  <- descendantListDefinition
    label'        <- optional newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure $ NewickNode descendants' label' branchLength'

descendantListDefinition :: MonadParsec s m Char => m [NewickNode]
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace

subtreeDefinition :: MonadParsec s m Char => m NewickNode
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition

newickLeafDefinition :: MonadParsec s m Char => m NewickNode
newickLeafDefinition = do
    label'        <- newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure . NewickNode [] (Just label') $ branchLength'

newickLabelDefinition :: MonadParsec s m Char => m String
newickLabelDefinition = (quotedLabel <|> unquotedLabel) <* whitespace

-- | We use a recursive parsing technique to handle the quoted escape sequence
--   of two single quotes ("''") to denote an escaped quotation character 
--   in the quoted label rather than signifying the end of the quoted label
quotedLabel :: MonadParsec s m Char => m String
quotedLabel = do
    _ <- char '\''
    x <- quotedLabelData
    case filter (not.isSpace) x of
      [] -> fail $ "Blank quoted identifier found. The identifier '"++x++"' is not valid"
      _  -> pure x
  where 
    quotedLabelData = do
      prefix <- many (noneOf $ '\'':invalidQuotedLabelChars)
      _      <- char '\'' 
      suffix <- optional (char '\'' <:> quotedLabelData)
      case suffix of
        Just y  -> pure $ prefix ++ y
        Nothing -> pure prefix

-- | The following characters are not allowed in a newick unquoted label:
--   " \r\n\t\v\b':;,()[]<>"
--   We disallow the '<' & '>' characters in unquoted labels in all newick 
--   file formats because they would interfere with the parsing of Foreset 
--   Extended Newick file types. The '<' & '>' characters are technically
--   allowed in an unquoted newick label according to the Gary Olsen
--   interpretation of the standard Newick format and the Extended Newick
--   format. However, if a user really want to put '<' & '>' characters in
--   a node label, they can always put such characters in a quoted label.
unquotedLabel :: MonadParsec s m Char => m String
unquotedLabel = some $ noneOf invalidUnquotedLabelChars

requiresQuotedLabelChars :: String
requiresQuotedLabelChars = " ':;,()[]<>"

invalidQuotedLabelChars :: String
invalidQuotedLabelChars = "\r\n\t\f\v\b"

invalidUnquotedLabelChars :: String
invalidUnquotedLabelChars = invalidQuotedLabelChars ++ requiresQuotedLabelChars

branchLengthDefinition :: MonadParsec s m Char => m Double
branchLengthDefinition = symbol (char ':') *> symbol double

trimmed :: MonadParsec s m Char => m a -> m a
trimmed x = whitespace *> x <* whitespace

symbol  :: MonadParsec s m Char => m a -> m a
symbol  x = x <* whitespace

whitespace :: MonadParsec s m Char => m ()
whitespace = try commentDefinition <|> space
  where
    commentDefinition :: MonadParsec s m Char => m ()
    commentDefinition = space *> some (comment commentStart commentEnd *> space) >> pure ()
    commentStart, commentEnd :: MonadParsec s m Char => m String
    commentStart = string "[" <?> "\"[\" comment start"
    commentEnd   = string "]" <?> "\"]\" comment end"

joinNonUniqueLabeledNodes :: MonadParsec s m Char => NewickNode -> m NewickNode
joinNonUniqueLabeledNodes root = joinNonUniqueLabeledNodes' [] root
  where
    -- We first fold over the Newick Tree to collect all labeled nodes and 
    -- combine thier descendant lists. We use this Map of Newick labels to
    -- combined descendant lists for substituting labeled node descendants
    -- in a second pass over the Newick Tree.
    joinedNodes :: Map String [NewickNode]
    joinedNodes = foldl joinNodes empty labeledNodes
      where
        labeledNodes           = filter (isJust . newickLabel) $ toList' root 
        joinNodes :: Map String [NewickNode] -> NewickNode -> Map String [NewickNode]
        joinNodes mapping node = insertWith (++) (fromJust $ newickLabel node) (descendants node) mapping
        toList' node = node : (concat . fmap toList' . descendants) node
    -- When transforming the Newick Tree to the Newick Network by joining 
    -- identically labeled nodes, there exists the possiblily that a directed 
    -- cycle is defined in the tree which will result in infinite recursion 
    -- during the transformation process (and probably erroneous processing
    -- in subsequent graph computations). To prevent this we track previously
    -- processed nodes via a stack to detect cycles. Upon cycle detection we
    -- return a Left value of type Either ParseError NewickNode to represent 
    -- a parse error. It is assumed that cycles are note permitted in our
    -- PhyloGraph data structures.
    joinNonUniqueLabeledNodes' :: MonadParsec s m Char => [Maybe String] -> NewickNode -> m NewickNode
    joinNonUniqueLabeledNodes' stack node
      | hasCycle      = fail cycleError
      | null children = pure $ newNode []
      | otherwise     = resultNode children
      where
        label      = newickLabel node
        joinedList = label >>= (`lookup` joinedNodes)
        children   = fromMaybe (descendants node) joinedList
        gatherList = sequence . fmap (joinNonUniqueLabeledNodes' stack')
        resultNode = liftM newNode . gatherList
        newNode x  = NewickNode x label (branchLength node)
        stack'     = label : stack
        hasCycle   = isJust label
                  && (not . null . dropWhile (/=label)) stack
        cycle'     = (label : takeWhile (/=label) stack) ++ [label]
        cycleError = init $ unlines -- we use init to remove trailing newline
                   [ "Cycle detected in Newick tree definition"
                   , prettyErr cycle'
                   ]
        prettyErr  = intercalate " -> " . fmap show
