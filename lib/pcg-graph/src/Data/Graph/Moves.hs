{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments   #-}

module Data.Graph.Moves where


import Data.Graph.Type
import Data.Graph.Hash
import Control.Lens
import Data.Graph.NodeContext
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

interchangeSubGraphs
  :: (HasHashValue c Int)
   => Graph f c e n (Graph f c e n t)
  -> Graph f c e n (Graph f c e n t)
  -> Int
  -> Int
  -> Graph f c e n (Graph f c e n t)
interchangeSubGraphs graph1 graph2 leafInd1 leafInd2 =
  let
    subGraph1 = graph1 ^. _leafReferences . (singular (ix leafInd1)) . _nodeData
    subGraph2 = graph2 ^. _leafReferences . (singular (ix leafInd2)) . _nodeData
    hashSubGraph1 = subGraph1  ^. _cachedData . _hashValue
    hashSubGraph2 = subGraph2  ^. _cachedData . _hashValue
  in
    if hashSubGraph1 == hashSubGraph2
      then
        graph1 & _leafReferences
               %~  V.modify
                     \v -> MV.modify v (& _nodeData .~ subGraph2) leafInd1
      else
        graph1
