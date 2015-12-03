-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exploritory types for Graph representations
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph where

import           Data.IntSet   hiding (toList,foldr)
import           Data.Sequence hiding (fromList)
import qualified Data.Sequence as S   (fromList)
import           Data.Vector   hiding (fromList,foldr)
import qualified Data.Vector   as V   (fromList)

type Edges = (IntSet,IntSet)
data AnnotatedNode  
   = AN
   { label     :: Maybe String
   , states    :: Int --Obviously not an Int
   , localCost :: Double
   , totalCost :: Double 
   }

type StrictNetwork = Vector Edges
type StrictForest  = [StrictNetwork]
data StrictGraph   = SG StrictForest (Vector AnnotatedNode)
type FluidNetwork  = Seq Edges
type FluidForest   = [FluidNetwork]
data FluidGraph    = FG FluidForest  (Seq    AnnotatedNode)

class Graphable a where
  toStrictGraph :: a -> StrictGraph
  toStrictGraph = (\(FG f a) -> SG (s2v <$> f) (s2v a)) . toFluidGraph
  toFluidGraph  :: a -> FluidGraph
  toFluidGraph  = (\(SG f a) -> FG (v2s <$> f) (v2s a)) . toStrictGraph

v2s :: Vector a -> Seq a
v2s = S.fromList . toList
s2v :: Seq a -> Vector a
s2v = V.fromList . foldr (:) []

{-
class PhyloGraph a where
	bitPack  :: a -> ???
	optemize :: a -> ???

-}
