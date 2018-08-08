{-# LANGUAGE FlexibleContexts #-}

module Test.Custom (module X, nodeInvariantHolds) where

import           Bio.PhyloGraph.Network
import           Test.Custom.Constructions           as X
import           Test.Custom.Parse                   as X
import           Test.Custom.Tree                    as X
import           Test.Custom.Types                   ()
import           Test.QuickCheck.Arbitrary.Instances ()

nodeInvariantHolds :: Network t n => (n -> Bool) -> t -> Bool
nodeInvariantHolds invariant inputTree = checkInvariant (root inputTree)
  where
    checkInvariant node = invariant node && and (checkInvariant <$> children node inputTree)
