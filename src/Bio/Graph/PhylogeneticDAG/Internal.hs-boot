------------------------------------------------------------------------------
--
-- A .hs-boot file for sharing the PhylogeneticDAG2 type.
--
-----------------------------------------------------------------------------

module Bio.Graph.PhylogeneticDAG.Internal
  ( PostorderContextualData(..)
  , PhylogeneticDAG2(..)
  ) where

import Analysis.Parsimony.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import Data.Vector
import           Data.HashMap.Lazy               (HashMap)
import Data.List.NonEmpty
import Bio.Graph.Node
import Bio.Metadata.Dynamic

type EdgeReference = (Int, Int)

data PostorderContextualData t = PostorderContextualData
  { virtualNodeMapping    :: HashMap EdgeReference (ResolutionCache t)
  , contextualNodeDatum   :: Vector (HashMap EdgeReference (ResolutionCache t))
  , minimalNetworkContext :: Maybe 
                               (NonEmpty 
                                 ( TraversalTopology
                                 , Double
                                 , Double
                                 , Double
                                 , Vector (NonEmpty TraversalFocusEdge)
                                 )
                               )
  }


data  PhylogeneticDAG2 m e n u v w x y z
    = PDAG2
    { phylogeneticForest :: ReferenceDAG
                              (PostorderContextualData (CharacterSequence u v w x y z))
                              e
                              (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
    , columnMetadata     :: MetadataSequence m
    }

