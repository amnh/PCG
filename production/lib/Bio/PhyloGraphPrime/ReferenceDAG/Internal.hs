-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ReferenceDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.PhyloGraphPrime.ReferenceDAG.Internal where

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty   (NonEmpty)
import           Data.IntMap          (IntMap)
import qualified Data.IntMap   as IM
import           Data.IntSet          (IntSet)
import           Data.Key
import           Data.MonoTraversable
import           Data.Vector          (Vector)
import qualified Data.Vector   as V
import           Prelude       hiding (lookup)

-- |
-- A constant time access representation of a directed acyclic graph.
-- 
data ReferenceDAG e n
   = RefDAG
   { references :: Vector (IndexData e n)
   , rootRefs   :: NonEmpty Int
   , graphData  :: GraphData
   }


data IndexData e n
   = IndexData
   { nodeDecoration :: n
   , parentRefs     :: IntSet
   , childRefs      :: IntMap e
   }

-- | Annotations which are global to the graph
data GraphData
   = GraphData
   { cost :: Double
   }


-- | (✔)
instance Bifunctor ReferenceDAG where

    bimap f g dag =
        RefDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (IndexData node parentRefs' childRefs') = IndexData (g node) parentRefs' $ f <$> childRefs'


-- | (✔)
instance Functor (ReferenceDAG e) where

    fmap f dag =
        RefDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (IndexData node parentRefs' childRefs') = IndexData (f node) parentRefs' childRefs'


-- | Build the graph functionally from a generating function.
unfoldDAG :: (b -> (n, [(e,b)])) -> b -> ReferenceDAG e n
unfoldDAG f root = undefined


newtype NodeRef = NR Int deriving (Eq, Enum)
                   
-- |
-- Represents the most relaxed phylogentic graph structure.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * the graph contains one or more root nodes
--  * All nodes have at most in-degree 2
--  * All nodes have out-degree 0 or out-degree 2
class PhylogeneticComponent (ReferenceDAG e n) NodeRef e n where

    parents   i dag = fmap toEnum . otoList . parentRefs $ references dag V.! fromEnum i
 
    children  i dag = fmap toEnum . IM.keys . childRefs  $ references dag V.! fromEnum i

    roots           = fmap toEnum . rootRefs

    leaves          = foldMapWithKey f . references
      where
        f i x
          | null $ childRefs x = mempty
          | otherwise          = toEnum i

    nodeCount       = length . references

    nodeDatum i dag = nodeDecoration $ references dag V.! fromEnum i

    edgeDatum (i,j) dag =  fromEnum j `lookup` childRefs (references dag V.! fromEnum i)

    -- |
    -- A node satisfying:
    --  * In-degree  of 2 or more
    --  * Out-degree of 2 or more
    --  * A least 2 parent nodes have ancestoral paths to different root nodes
    isComponentNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree  of 2 or more
    --  * Out-degree of 2 or more
    --  * All parent nodes have ancestoral paths to a single root node
    isNetworkNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree  of 1
    --  * Out-degree of 2
    isTreeNode

    -- |
    -- A node satisfying:
    --  * Out-degree 0
    isLeafNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree 0
    isRootNode :: i -> t -> Bool

    -- |
    -- Performs a softwire resolution of all /component/ nodes into a collection
    -- of all resulting networks. The resulting size of the collection is equal
    -- to /2^n/ where /n/ is the number of component nodes in the
    -- 'PhylogeneticComponent'.
    networkResolutions :: t -> NonEmpty t


-- |
-- Represents a more constrained phylogentic graph structure.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * The graph contains /exactly one/ root nodes
--  * All nodes have at most in-degree 2
--  * All nodes have out-degree 0 or out-degree 2
class PhylogeneticNetwork t i e n | t -> i, t -> n, t -> e where

    root  :: t -> i

    -- |
    -- Performs a softwire resolution of all /network/ nodes into a collection
    -- of all resulting trees. The resulting size of the collection is equal
    -- to /2^n/ where /n/ is the number of network nodes in the
    -- 'PhylogenetiNetwork'.
    treeResolutions :: t -> NonEmpty t


-- |
-- Represents the most constrained phylogentic graph structure.
-- The constraints correlate to a binary tree.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * The graph contains /exactly one/ root nodes
--  * All nodes have /exactly/ in-degree 1
--  * All nodes contain out-degree 0 or out-degree 2
class PhylogeneticTree t i e n | t -> i, t -> n, t -> e where

    parent :: i -> t -> Maybe i

    bifurcation :: i -> t -> Maybe (i,i)
