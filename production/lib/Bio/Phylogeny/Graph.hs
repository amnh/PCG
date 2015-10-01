module Bio.Phylogeny.Graph where

import Data.IntSet
import qualified Data.Sequence as S
import qualified Data.Vector   as V


type Edges = (IntSet,IntSet)
data AnnotatedNode  
   = AN
   { label     :: Maybe String
   , states    :: Int --Obviously not an Int
   , localCost :: Double
   , totalCost :: Double 
   }

type StrictNetwork = V.Vector Edges
type StrictForest  = [StrictNetwork]
data StrictGraph   = SG StrictForest (V.Vector AnnotatedNode)
type FluidNetwork  = S.Seq Edges
type FluidForest   = [FluidNetwork]
data FluidGraph    = FG FluidForest  (S.Seq    AnnotatedNode)

class Graphable a where
	toStrictGraph :: a -> StrictGraph
	toStrictGraph = \(FG f a) -> FG (s2v <$> f) (s2v a) . toFluidGraph
	toFluidGraph  :: a -> FluidGraph
	toFluidGraph  = \(SG f a) -> FG (v2s <$> f) (v2s a) . toStrictGraph

v2s = S.fromList . V.toList
s2v = V.fromList . S.fromList

{-
class PhyloGraph a where
	bitPack  :: a -> ???
	optemize :: a -> ???

-}