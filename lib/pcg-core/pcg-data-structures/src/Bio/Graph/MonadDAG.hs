-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.MonadDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Graph.MonadDAG where

import Data.List.NonEmpty


newtype NodeRef = NR Int deriving (Eq)


class MonadDAG t e n where

    parents   :: NodeRef -> t [NodeRef]

    children  :: NodeRef -> t [NodeRef]

    neighbors :: NodeRef -> t [NodeRef]

    roots     :: t [NodeRef]

    leaves    :: t [NodeRef]

    nodeCount :: t Int

    nodeDatum :: NodeRef -> t n

    edgeDatum :: (NodeRef, NodeRef) -> t (Maybe e)

    networkResolutions :: t -> NonEmpty t
