-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Newick.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions and types for parsing Newick tree files into a topological tree structure.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module File.Format.Newick.Internal
  ( NewickForest
  , NewickNode(..)
  -- * Queries
  , branchLength
  , descendants
  , newickLabel
  , isLeaf
  -- * Other
  , mapLeafLabels
  , newickNode
  , renderNewickForest
  ) where


import           Control.DeepSeq    (NFData)
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.String        (IsString (fromString))
import           Data.Text          (Text)
import           Data.Text.Short    (ShortText, toString)
import qualified Data.Text.Short    as T
import           Data.Tree
import           Data.Vector        (Vector, fromList, fromListN)
import           GHC.Generics       (Generic)


{----
  - The Newick file format was developed by an informal committee meeting at
  - Newick's seafood restaurant. The grammar definition of the Newick format
  - was never formally specified, but Gary Olsen's interpretation of the
  - original newick format has been documented here:
  - http://evolution.genetics.washington.edu/phylip/newick_doc.html
  -
  - After over two decades of informal usage, the Extended Newick file format
  - was proposed in a BCM publication which allowed node labels to be non-
  - unique and merged to a single node with shared ancestors and descendants.
  - This allowed for easy manual annotating of phylogentic trees.
  -
  - Another half decade later, the Forest Extended Newick was proposed by
  - Professor Wheeler to model collections of disjoint phylogenetic trees.
  - This new format allowed grouping many Extended Newick trees into a
  - forest to be analyzed collectively.
  -
  - This parser correctly parses both Newick file formats, and the super set
  - Extended Newick filed format.
  -}


-- |
-- One or more trees in a "Phylogenetic Forest".
type NewickForest = NonEmpty NewickNode


-- |
-- A node in a "Phylogenetic Forest"
data  NewickNode
    = NewickNode
    { childNodes     :: {-# UNPACK #-} !(Vector NewickNode)
    , internalName   :: {-# UNPACK #-} !ShortText
    , internalLength :: !(Maybe Rational)
    } deriving (Eq, Generic, NFData, Ord)


-- |
-- Apply a transformation over the leaf labels of a 'newickNode'.
mapLeafLabels :: (ShortText -> ShortText) -> NewickNode -> NewickNode
mapLeafLabels f (NewickNode d x y) = NewickNode (mapLeafLabels f <$> d) x y


-- |
-- Retrieve the direct descendants of the 'NewickNode'.
{-# INLINE descendants #-}
descendants :: NewickNode -> [NewickNode]
descendants (NewickNode x _ _) = toList x


-- |
-- Retrieve the label (if any) of the 'NewickNode'.
--
-- Will always return a @Just@ value for a leaf node.
--
-- > isLeaf ==> isJust . newickLabel
{-# INLINE newickLabel #-}
newickLabel :: NewickNode -> Maybe ShortText
newickLabel (NewickNode _ x _)
  | T.null x  = Nothing
  | otherwise = Just x


-- |
-- Retrieve the branch length (if any) of the 'NewickNode'.
{-# INLINE branchLength #-}
branchLength :: NewickNode -> Maybe Rational
branchLength (NewickNode _ _ x) = x


instance Semigroup NewickNode where

    {-# INLINEABLE (<>) #-}
    lhs <> rhs =
        NewickNode
        { childNodes     = fromListN 2 [lhs, rhs]
        , internalName   = ""
        , internalLength = Nothing
        }


instance Show NewickNode where

    show (NewickNode d n b) = fold [name, len, " ", show d]
      where
        name = (\x -> if null x then "Node" else x) $ toString n
        len  = maybe "" (\x -> ':' : show x) b


-- |
-- Renders the 'NewickForest' to a 'String'. If the forest contains a DAG with
-- in-degree  greater than one, then the shared subtree in a DAG will be rendered
-- multiple times.
{-# INLINEABLE renderNewickForest #-}
renderNewickForest :: NewickForest -> Text
renderNewickForest = fromString . drawForest . unfoldForest f . toList
  where
    f = (,) <$> maybe "X" toString . newickLabel <*> descendants


-- |
-- Smart constructor for a 'NewickNode' preseriving the invariant:
--
-- > null nodes ==> isJust . label
{-# INLINEABLE newickNode #-}
newickNode :: [NewickNode] -> Maybe ShortText -> Maybe Rational -> Maybe NewickNode
newickNode nodes label length' =
  case (nodes, label) of
    (  [], Nothing) -> Nothing
    _               -> Just $ NewickNode (fromList nodes) (fold label) length'


-- |
-- Determines whether a given 'NewickNode' is a leaf node in the tree.
{-# INLINEABLE isLeaf #-}
isLeaf :: NewickNode -> Bool
isLeaf (NewickNode x y _) = null x && not (T.null y)
