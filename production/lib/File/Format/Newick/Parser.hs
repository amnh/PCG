-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Newick
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function for parsing Newick tree files into a topological tree structure.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Newick.Parser where

import Data.Char                  (isSpace)
import Data.List                  (intercalate)
import Data.Map            hiding (filter,foldl,foldr,null)
import Data.Maybe                 (fromJust,fromMaybe,isJust)
import File.Format.Newick.Internal
import Prelude             hiding (lookup)
import Text.Megaparsec     hiding (label)
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim       (MonadParsec)

-- | Parses a stream producing a standard Newick tree
newickStandardDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')

-- | Parses a stream producing an extended Newick tree.
-- Directed cycles in extended Newick trees are not permitted.
newickExtendedDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickExtendedDefinition = newickStandardDefinition >>= joinNonUniqueLabeledNodes

-- | Parses a stream producing a forest of extended Newick trees.
newickForestDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickForest
newickForestDefinition = whitespace *> symbol (char '<') *> many newickExtendedDefinition <* symbol (char '>')

-- | Definition of a serialized Newick node consisiting of the node's descendants,
-- optional label, and optional branch length. Mutually recursive with 'subtreeDefinition '.
newickNodeDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickNodeDefinition = do
    descendants'  <- descendantListDefinition
    label'        <- optional newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure $ NewickNode descendants' label' branchLength'

-- | Parses one or more subtrees consisting of a single node or a further descendant list.
descendantListDefinition :: (MonadParsec e s m, Token s ~ Char) => m [NewickNode]
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace

-- | Definition of a Newick subtree consisting of either a single leaf node or a greater subtree.
-- Mutually recursive with 'newickNodeDefinition '.
subtreeDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition

-- | Definition of a sigle leaf node in a Newick tree. Must contain a node label.
-- Has no descendants be definition.
newickLeafDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickLeafDefinition = do
    label'        <- newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure . NewickNode [] (Just label') $ branchLength'

-- | Defines the label for a 'NewickNode' which can be either quoted or unquoted.
newickLabelDefinition :: (MonadParsec e s m, Token s ~ Char) => m String
newickLabelDefinition = (quotedLabel <|> unquotedLabel) <* whitespace

-- | We use a recursive parsing technique to handle the quoted escape sequence
--   of two single quotes ("''") to denote an escaped quotation character 
--   in the quoted label rather than signifying the end of the quoted label
quotedLabel :: (MonadParsec e s m, Token s ~ Char) => m String
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
unquotedLabel :: (MonadParsec e s m, Token s ~ Char) => m String
unquotedLabel = some $ noneOf invalidUnquotedLabelChars

-- | Characters which can ontly appear in a quoted 'NewickNode' label.
requiresQuotedLabelChars :: String
requiresQuotedLabelChars = " ':;,()[]<>"

-- | List of chacracters which __cannot__ appear in an /quoted/ label of a 'NewickNode'.
invalidQuotedLabelChars :: String
invalidQuotedLabelChars = "\r\n\t\f\v\b"

-- | List of chacracters which __cannot__ appear in an /unquoted/ label of a 'NewickNode'.
-- A superset of 'invalidQuotedLabelChars'.
invalidUnquotedLabelChars :: String
invalidUnquotedLabelChars = invalidQuotedLabelChars ++ requiresQuotedLabelChars

-- | Definition of a serialized branch length between two nodes in the Newick tree.
-- Since the Newick tree is impicitly rooted in it's serialization form, the 'branchLength'
-- of a given 'NewickNode' is the branch length itself and it's parent. Becomes non-sensical
-- with extended Newick trees that have nodes with "in-degree" greater than one.
branchLengthDefinition :: (MonadParsec e s m, Token s ~ Char) => m Double
branchLengthDefinition = symbol (char ':') *> symbol double

-- | Convinience combinator for stripping /leading and trailing/ whitespace from a combinator.
trimmed :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace

-- | Convinience combinator for stripping /trailing/ whitespace from a combinator.
symbol  :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol  x = x <* whitespace

-- | Definition of space between tokens which can be discarded. This includes spaces /and/ comments.
whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = try commentDefinition <|> space
  where
    commentDefinition :: (MonadParsec e s m, Token s ~ Char) => m ()
    commentDefinition = space *> some (comment commentStart commentEnd *> space) >> pure ()
    commentStart, commentEnd :: (MonadParsec e s m, Token s ~ Char) => m String
    commentStart = string "[" <?> "\"[\" comment start"
    commentEnd   = string "]" <?> "\"]\" comment end"

-- | Joins the nodes of an extended Newick tree which share the same label.
-- Directed cycles from the tree's implicit root will result in a 'ParseError'.
joinNonUniqueLabeledNodes :: (MonadParsec e s m, Token s ~ Char) => NewickNode -> m NewickNode
joinNonUniqueLabeledNodes root = joinNonUniqueLabeledNodes' [] root
  where
    -- We first fold over the Newick Tree to collect all labeled nodes and 
    -- combine thier descendant lists. We use this Map of Newick labels to
    -- combined descendant lists for substituting labeled node descendants
    -- in a second pass over the Newick Tree.
    joinedNodes :: Map String [NewickNode]
    joinedNodes = foldl joinNodes mempty labeledNodes
      where
        labeledNodes           = filter (isJust . newickLabel) $ toList' root 
        joinNodes :: Map String [NewickNode] -> NewickNode -> Map String [NewickNode]
        joinNodes mapping node = insertWith (++) (fromJust $ newickLabel node) (descendants node) mapping
        toList' node = node : ((=<<) toList' . descendants) node
    -- When transforming the Newick Tree to the Newick Network by joining 
    -- identically labeled nodes, there exists the possiblily that a directed 
    -- cycle is defined in the tree which will result in infinite recursion 
    -- during the transformation process (and probably erroneous processing
    -- in subsequent graph computations). To prevent this we track previously
    -- processed nodes via a stack to detect cycles. Upon cycle detection we
    -- return a Left value of type Either ParseError NewickNode to represent 
    -- a parse error. It is assumed that cycles are note permitted in our
    -- PhyloGraph data structures.
    joinNonUniqueLabeledNodes' :: (MonadParsec e s m, Token s ~ Char) => [Maybe String] -> NewickNode -> m NewickNode
    joinNonUniqueLabeledNodes' stack node
      | hasCycle      = fail cycleError
      | null children = pure $ newNode []
      | otherwise     = resultNode children
      where
        label      = newickLabel node
        joinedList = label >>= (`lookup` joinedNodes)
        children   = fromMaybe (descendants node) joinedList
        gatherList = sequence . fmap (joinNonUniqueLabeledNodes' stack')
        resultNode = fmap newNode . gatherList
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
