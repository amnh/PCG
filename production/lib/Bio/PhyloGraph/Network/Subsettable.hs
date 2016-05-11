-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Network.Subsettable
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for a subsettable network, one where you can grab and append subtrees
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Network.Subsettable where


-- | A subsettable network is one where subtrees can be appended or accessed with their topology intact
class SubsettableNetwork t n | t -> n where
    appendSubtree :: t -> t -> n -> t
    accessSubtree :: t -> n -> t
