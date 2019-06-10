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
  -- TODO: (Deprecated)
  , newickLabelShort
  ) where


import Control.DeepSeq      (NFData)
import Data.Foldable
import Data.List.NonEmpty   (NonEmpty(..))
import Data.String          (IsString (fromString))
import Data.Text            (Text)
import Data.Text.Short      (ShortText, toString)
import Data.Tree
import Data.Vector.NonEmpty (Vector, fromNonEmpty)
import GHC.Generics         (Generic)


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


data NewickNode
    = NewickLeaf {-# UNPACK #-} !ShortText !(Maybe Rational)
    | NewickInternal
    { childNodes     :: {-#UNPACK #-} !(Vector NewickNode)
    , internalName   :: !(Maybe ShortText)
    , internalLength :: !(Maybe Rational)
    } deriving (Eq, Generic, NFData, Ord)


mapLeafLabels :: (ShortText -> ShortText) -> NewickNode -> NewickNode
mapLeafLabels f (NewickLeaf       x y) = NewickLeaf                             (f x) y
mapLeafLabels f (NewickInternal d x y) = NewickInternal (mapLeafLabels f <$> d)    x  y


{-# INLINE descendants #-}
descendants :: NewickNode -> [NewickNode]
descendants  NewickLeaf {}         = mempty
descendants (NewickInternal x _ _) = toList x


{-# INLINE newickLabel #-}
newickLabel :: NewickNode -> Maybe ShortText
newickLabel (NewickLeaf       x _) = Just x
newickLabel (NewickInternal _ x _) = x


{-# INLINE branchLength #-}
branchLength :: NewickNode -> Maybe Rational
branchLength (NewickLeaf       _ x) = x
branchLength (NewickInternal _ _ x) = x


instance Semigroup NewickNode where

    lhs <> rhs =
        NewickInternal
        { childNodes     = fromNonEmpty $ lhs:|[rhs]
        , internalName   = Nothing
        , internalLength = Nothing
        }


instance Show NewickNode where

    show node =
        case node of
          NewickLeaf       n b -> fold [name (Just n), len b]
          NewickInternal d n b -> fold [name       n , len b, " ", show d]
      where
        name n = maybe "Node" toString n
        len  b = maybe "" (\x -> ':' : show x) b


-- TODO: No String, only ShortText
{-
-- |
-- A node in a "Phylogenetic Forest"
data NewickNode
   = NewickNode
   { descendants  :: [NewickNode] -- ^ List of node's children, leaf nodes are empty lists
   , newickLabel  :: Maybe String -- ^ The node's possibly included label, leaf nodes will always be Just-valued
   , branchLength :: Maybe Double -- ^ The node's possibly included branch length
   } deriving (Eq, Generic, NFData, Ord)


instance Show NewickNode where

    show (NewickNode d n b) = name <> len <> " " <> show d
      where
        name = maybe "Node" show n
        len  = maybe "" (\x -> ':' : show x) b


instance Semigroup NewickNode where

    lhs <> rhs =
        NewickNode
        { descendants  = [lhs,rhs]
        , newickLabel  = Nothing
        , branchLength = Nothing
        }
-}


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
    (  [], Just x ) -> Just $ NewickLeaf x length'
    (x:xs,      _ ) -> Just $ NewickInternal (fromNonEmpty $ x:|xs) label length' 


-- |
-- Determines whether a given 'NewickNode' is a leaf node in the tree.
{-# INLINEABLE isLeaf #-}
isLeaf :: NewickNode -> Bool
isLeaf NewickLeaf {} = True
isLeaf _             = False


-- |
-- Extracts a newick label (if it exists) and converts to ShortText
{-# INLINE newickLabelShort #-}
newickLabelShort :: NewickNode -> Maybe ShortText
newickLabelShort = newickLabel 
