module Data.Graph.Postorder.NetworkEdgeQuantification where


import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Data.Graph.TopologyRepresentation
import Data.Graph.Type
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Graph.Postorder.Resolution (Resolution, HasLeafSetRepresentation(..))
import qualified Data.Graph.Postorder.Resolution as Resolution
import Data.Monoid
import Control.Lens
import Data.Bits
import Data.Graph.Sequence.Class
import Data.Graph.Postorder.DynamicTraversalFoci
import Data.Graph.Indices




assignPunitiveNetworkEdgeCost
  :: ()
  => Graph f c e n t
  -> ( TraversalEdgeCostInfo
     , Graph f c e n t
     )
assignPunitiveNetworkEdgeCost = undefined


data TraversalEdgeCostInfo = TraversalEdgeCostInfo
  { networkTopology :: NetworkTopology
  }

--extractMinimalDisplayForestInfo
--  :: Foldable f
--  =>

createForestContext
  :: (Foldable f)
  => MetadataSequence block m
  -> Vector (Vector TraversalFoci)
  -> f (Resolution (CharacterSequence block))
  -> TraversalEdgeCostInfo
createForestContext metaSeq rootingTraversalFoci resolutions = undefined
  where
    rootCount = length resolutions
    unbiasedRootingEdge = undefined

data CharacterMinimizationInfo = CharacterMinimizationInfo
  { rootCost :: Double
  , blockCost :: Double
  , traversalEdges :: NonEmpty EdgeIndex
  }

data BlockMinimizationInfo = BlockMinimizationInfo
  { topology         :: NetworkTopology
  , charMinimization :: Vector (CharacterMinimizationInfo)
  }
  


-- |
-- This function takes a collection of resolutions from a root node and makes sure
-- that if there are more than one resolution then together the resolutions do
-- not overlap and that the resolutions include all leaves in the root subtree.
isValidDisplayForest ::  NonEmpty (Resolution cs) -> Bool
isValidDisplayForest (r :| []) = True
isValidDisplayForest resolutions@(_ :| (_ : _)) =
  let
    rs = NonEmpty.toList resolutions
    allDisjoint
      = getAll
      . foldMap (All . uncurry Resolution.disjointResolutions)
      $ zip rs (tail rs)

    completeCoverage =
      let bitVal = foldMap (view _leafSetRepresentation) resolutions
      in  complement (bitVal `xor` bitVal) == bitVal
  in
    completeCoverage && allDisjoint
  

