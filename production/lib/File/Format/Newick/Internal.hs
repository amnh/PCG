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

{-# LANGUAGE MultiParamTypeClasses #-}

module File.Format.Newick.Internal
  ( NewickForest
  , NewickNode(..)
  , isLeaf
  , newickNode
  , renderNewickForest
  ) where


import           Data.Tree
import           Data.Maybe
--import qualified Bio.PhyloGraph.Network as N
--import           Data.List
import           Data.List.NonEmpty  (NonEmpty, toList)
import           Data.Semigroup


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
data NewickNode
   = NewickNode
   { descendants  :: [NewickNode] -- leaf nodes are empty lists
   , newickLabel  :: Maybe String -- leaf nodes will always be Just
   , branchLength :: Maybe Double
   } deriving (Eq,Ord)


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


-- |
-- Renders the 'NewickForest' to a 'String'. If the forest contains a DAG with
-- in-degree  greater than one, then the shared subtree in a DAG will be rendered
-- multiple times. 
renderNewickForest :: NewickForest -> String
renderNewickForest = drawForest . unfoldForest f . toList
  where
    f = (,) <$> fromMaybe "X" . newickLabel <*> descendants


-- |
-- Smart constructor for a 'NewickNode' preseriving the invariant:
--
-- > null nodes ==> isJust . label
newickNode :: [NewickNode] -> Maybe String -> Maybe Double -> Maybe NewickNode
newickNode nodes label length'
  | null nodes && isNothing label = Nothing
  | otherwise = Just $ NewickNode nodes label length'


-- |
-- Determines whether a given 'NewickNode' is a leaf node in the tree.
isLeaf :: NewickNode -> Bool
isLeaf node = (null . descendants) node && (isJust . newickLabel) node

{-
instance N.Network NewickNode NewickNode where

    root     t   = t

    children n _ = descendants n

    nodeIsLeaf   n _ = isLeaf n

    nodeIsRoot   n t = null $ N.parents n t

    parents node tree 
      | node `elem` descendants tree = nub $ tree : foldr (\n acc -> N.parents node n ++ acc) [] (descendants tree)
      | otherwise = nub $ foldr (\n acc -> N.parents node n ++ acc) [] (descendants tree)

    update tree new = 
      let 
          updateHere = foldr (\n acc -> if match n acc then upOne n acc else acc) tree new
          updateRest = updateHere { descendants = (`N.update` new) <$> descendants updateHere }
      in updateRest
      where
        match newNode t = newickLabel t == newickLabel newNode
        upOne newNode t = t {descendants = descendants newNode, branchLength = branchLength newNode}

    numNodes = tallyNodes
      where
        tallyNodes :: NewickNode -> Int
        tallyNodes = succ . sum . fmap tallyNodes . descendants

    addNode t n = t { descendants = n : descendants t }
-}
