{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Node.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.Node.Class where

import Bio.Graph.Node.Internal (PhylogeneticNode2(..), ResolutionInformation, ResolutionCache)
import Control.Lens.Lens (Lens', lens)

-- |
-- A 'Lens' for the 'resolutions' field.
class HasResolutions s a | s -> a where
  _resolutions :: Lens' s a
{-# SPECIALISE _resolutions :: Lens' (PhylogeneticNode2 s n) (ResolutionCache n) #-}

instance HasResolutions (PhylogeneticNode2 s n) (ResolutionCache s) where
  {-# INLINE _resolutions #-}
  _resolutions = lens resolutions (\p r -> p {resolutions = r})

-- |
-- A 'Lens' for the 'nodeDecorationDatum' field.
-- Note: The field accessor is currently called nodeDecorationDatum2.
class HasNodeDecorationDatum s a | s -> a where
  _nodeDecorationDatum  :: Lens' s a
{-# SPECIALISE _nodeDecorationDatum :: Lens' (PhylogeneticNode2 s n) (ResolutionCache n) #-}

instance HasNodeDecorationDatum (PhylogeneticNode2 s n) n where
  {-# INLINE _nodeDecorationDatum #-}
  _nodeDecorationDatum = lens nodeDecorationDatum2 (\p n -> p {nodeDecorationDatum2 = n})



    -- ( EdgeSet
    -- , NewickSerialization()
    -- , PhylogeneticNode (..)
    -- , PhylogeneticNode2(..)
    -- , ResolutionCache
    -- , ResolutionInformation(..)
    -- , SubtreeLeafSet()
    -- , hasLeafSet
    -- , addEdgeToEdgeSet
    -- , singletonEdgeSet
    -- , singletonNewickSerialization
    -- , singletonSubtreeLeafSet
    -- , pNode2
    -- ) where


-- import Bio.Graph.Node.Internal
-- import Control.Lens


-- -- |
-- -- A 'Lens' for the 'characterWeight' field
-- class HasCharacterWeight s a | s -> a where

--     {-# MINIMAL characterWeight #-}
--     characterWeight :: Lens' s a
