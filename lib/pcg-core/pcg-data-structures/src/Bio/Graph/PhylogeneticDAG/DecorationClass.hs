{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Bio.Graph.PhylogeneticDAG.DecorationClass where

import Bio.Graph.ReferenceDAG.Internal
import Control.Lens.Lens               (Lens')
import Data.MonoTraversable

type IndexedContainer c = (Functor c, Foldable c)

class ( IndexedContainer (RefContainer g)
      , IndexedContainer (RootContainer g)
      )
  => ReferenceDAG g where
  type RefContainer  g :: * -> *
  type RootContainer g :: * -> *

  type NodeDecoration g :: *
  type GraphMetadata  g :: *

  references :: Lens' g (RefContainer g (IndexData e n))
  rootRefs   :: Lens' g ((RootContainer g) Int           )
  graphData  :: Lens' g (GraphData (GraphMetadata g))
