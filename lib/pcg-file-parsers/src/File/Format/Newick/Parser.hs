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

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module File.Format.Newick.Parser
  ( branchLengthDefinition
  , descendantListDefinition
  , quotedLabel
  , newickExtendedDefinition
  , newickForestDefinition
  , newickLabelDefinition
  , newickLeafDefinition
  , newickStandardDefinition
  , requiresQuotedLabelChars
  , unquotedLabel
  ) where

import Control.Applicative.Combinators.NonEmpty
import Data.Char                   (isSpace)
import Data.Foldable
import Data.Functor                (void)
import Data.List                   (intercalate)
import Data.List.NonEmpty          (some1)
import Data.List.NonEmpty          (NonEmpty(..))
import Data.Map                    hiding (filter, fold, foldl', null)
import Data.Maybe                  (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.String
import Data.Text.Short             (ShortText)
import Data.Vector.NonEmpty        (fromNonEmpty)
import File.Format.Newick.Internal
import Prelude                     hiding (lookup)
import Text.Megaparsec             hiding (label, sepBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer  (scientific, skipBlockCommentNested)
import Text.Megaparsec.Custom


-- |
-- Parses a stream producing a standard Newick tree
{-# INLINEABLE newickStandardDefinition #-}
newickStandardDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')


-- |
-- Parses a stream producing an extended Newick tree.
-- Directed cycles in extended Newick trees are not permitted.
{-# INLINEABLE newickExtendedDefinition #-}
newickExtendedDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickExtendedDefinition = newickStandardDefinition >>= joinNonUniqueLabeledNodes


-- |
-- Parses a stream producing a forest of extended Newick trees.
{-# INLINEABLE newickForestDefinition #-}
newickForestDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickForest
newickForestDefinition = whitespace *> symbol (char '<') *> some1 newickExtendedDefinition <* symbol (char '>')


-- |
-- Definition of a serialized Newick node consisiting of the node's descendants,
-- optional label, and optional branch length. Mutually recursive with
-- 'subtreeDefinition '.
{-# INLINEABLE newickNodeDefinition #-}
newickNodeDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickNodeDefinition = do
    descendants'  <- fromNonEmpty <$> descendantListDefinition
    label'        <- optional newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure $ NewickInternal descendants' label' branchLength'


-- |
-- Parses one or more subtrees consisting of a single node or a further
-- descendant list.
{-# INLINEABLE descendantListDefinition #-}
descendantListDefinition :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty NewickNode)
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace


-- |
-- Definition of a Newick subtree consisting of either a single leaf node or a
-- greater subtree. Mutually recursive with 'newickNodeDefinition'.
{-# INLINEABLE subtreeDefinition #-}
subtreeDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition


-- |
-- Definition of a sigle leaf node in a Newick tree. Must contain a node label.
-- Has no descendants be definition.
{-# INLINEABLE newickLeafDefinition #-}
newickLeafDefinition :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickLeafDefinition = do
    label'        <- newickLabelDefinition
    branchLength' <- optional branchLengthDefinition
    pure . NewickLeaf label' $ branchLength'


-- |
-- Defines the label for a 'NewickNode' which can be either quoted or unquoted.
{-# INLINEABLE newickLabelDefinition #-}
newickLabelDefinition :: (MonadParsec e s m, Token s ~ Char) => m ShortText
newickLabelDefinition = (quotedLabel <|> unquotedLabel) <* whitespace


-- |
-- We use a recursive parsing technique to handle the quoted escape sequence
-- of two single quotes ("''") to denote an escaped quotation character
-- in the quoted label rather than signifying the end of the quoted label
{-# INLINEABLE quotedLabel #-}
quotedLabel :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m ShortText
quotedLabel = do
    _ <- char '\''
    x <- quotedLabelData
    case filter (not . isSpace) x of
      [] -> fail $ fold ["Blank quoted identifier found. The identifier '", x, "' is not valid"]
      _  -> pure $ fromString x
  where
    quotedLabelData = do
      prefix <- noneOfThese $ '\'':invalidQuotedLabelChars
      _      <- char '\''
      suffix <- optional . try $ char '\'' *> quotedLabelData
      pure $ let p = chunkToTokens (Proxy :: Proxy s) prefix
             in  case suffix of
                   Just y  -> p <> ('\'' : y)
                   Nothing -> p


-- |
-- The following characters are not allowed in a newick unquoted label:
-- " \r\n\t\v\b':;,()[]<>"
-- We disallow the '<' & '>' characters in unquoted labels in all newick
-- file formats because they would interfere with the parsing of Foreset
-- Extended Newick file types. The '<' & '>' characters are technically
-- allowed in an unquoted newick label according to the Gary Olsen
-- interpretation of the standard Newick format and the Extended Newick
-- format. However, if a user really want to put '<' & '>' characters in
-- a node label, they can always put such characters in a quoted label.
{-# INLINEABLE unquotedLabel #-}
unquotedLabel :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m ShortText
unquotedLabel =
  fromString . chunkToTokens (Proxy :: Proxy s) <$> noneOfThese invalidUnquotedLabelChars


-- |
-- Characters which can ontly appear in a quoted 'NewickNode' label.
{-# INLINE requiresQuotedLabelChars #-}
requiresQuotedLabelChars :: String
requiresQuotedLabelChars = " ':;,()[]<>"


-- |
-- List of chacracters which __cannot__ appear in an /quoted/ label of a
-- 'NewickNode'.
{-# INLINE invalidQuotedLabelChars #-}
invalidQuotedLabelChars :: String
invalidQuotedLabelChars = "\r\n\t\f\v\b"


-- |
-- List of chacracters which __cannot__ appear in an /unquoted/ label of a
-- 'NewickNode'. A superset of 'invalidQuotedLabelChars'.
{-# INLINE invalidUnquotedLabelChars #-}
invalidUnquotedLabelChars :: String
invalidUnquotedLabelChars = invalidQuotedLabelChars <> requiresQuotedLabelChars


-- |
-- Definition of a serialized branch length between two nodes in the Newick
-- tree. Since the Newick tree is impicitly rooted in it's serialization form,
-- the 'branchLength' of a given 'NewickNode' is the branch length itself and
-- it's parent. Becomes non-sensical with extended Newick trees that have nodes
-- with "in-degree" greater than one.
{-# INLINEABLE branchLengthDefinition #-}
branchLengthDefinition :: (MonadParsec e s m, Token s ~ Char) => m Rational
branchLengthDefinition = symbol (char ':') *> (toRational <$> symbol scientific)


-- |
-- Convinience combinator for stripping /leading and trailing/ whitespace from a
-- combinator.
{-# INLINE trimmed #-}
trimmed :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace


-- |
-- Convinience combinator for stripping /trailing/ whitespace from a combinator.
{-# INLINE symbol #-}
symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol x = x <* whitespace


-- |
-- Definition of space between tokens which can be discarded. This includes
-- spaces /and/ comments.
{-# INLINE whitespace #-}
whitespace :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = skipMany $ choice [ hidden spChar, hidden block ]
  where
    spChar = void spaceChar
    block  = skipBlockCommentNested (tokenToChunk proxy '[') (tokenToChunk proxy ']')
    proxy  = Proxy :: Proxy s


{-
whitespace = try commentDefinition <|> space
  where
    commentDefinition :: (MonadParsec e s m, Token s ~ Char) => m ()
    commentDefinition = space *> some (comment commentStart commentEnd *> space) >> pure ()
    commentStart, commentEnd :: (MonadParsec e s m, Token s ~ Char) => m String
    commentStart = string "[" <?> "\"[\" comment start"
    commentEnd   = string "]" <?> "\"]\" comment end"
-}

-- |
-- Joins the nodes of an extended Newick tree which share the same label.
-- Directed cycles from the tree's implicit root will result in a 'ParseError'.
{-# INLINE joinNonUniqueLabeledNodes #-}
joinNonUniqueLabeledNodes :: (MonadParsec e s m, Token s ~ Char) => NewickNode -> m NewickNode
joinNonUniqueLabeledNodes root = joinNonUniqueLabeledNodes' [] root
  where

    -- We first fold over the Newick Tree to collect all labeled nodes and
    -- combine thier descendant lists. We use this Map of Newick labels to
    -- combined descendant lists for substituting labeled node descendants
    -- in a second pass over the Newick Tree.
    joinedNodes :: Map ShortText [NewickNode]
    joinedNodes = foldl' joinNodes mempty labeledNodes
      where
        labeledNodes = filter (isJust . newickLabel) $ toList' root
        
        joinNodes :: Map ShortText [NewickNode] -> NewickNode -> Map ShortText [NewickNode]
        joinNodes mapping node = insertWith (<>) (fromJust $ newickLabel node) (descendants node) mapping
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
    joinNonUniqueLabeledNodes' :: (MonadParsec e s m, Token s ~ Char) => [Maybe ShortText] -> NewickNode -> m NewickNode
    joinNonUniqueLabeledNodes' stack node
      | hasCycle   = fail cycleError
      | otherwise  =
        case children of
          []   -> pure node
          x:xs -> resultNode . fromNonEmpty $ x:|xs
      where
        label      = newickLabel node
        joinedList = label >>= (`lookup` joinedNodes)
        children   = fromMaybe (descendants node) joinedList
        gatherList = traverse (joinNonUniqueLabeledNodes' stack')
        resultNode = fmap newNode . gatherList
        newNode x  = NewickInternal x label (branchLength node)
        stack'     = label : stack
        hasCycle   = isJust label
                  && (not . null . dropWhile (/=label)) stack
        cycle'     = (label : takeWhile (/=label) stack) <> [label]
        cycleError = init $ unlines -- we use init to remove trailing newline
                   [ "Cycle detected in Newick tree definition"
                   , prettyErr cycle'
                   ]
        prettyErr  = intercalate " -> " . fmap show
