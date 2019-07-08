{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Graph.Indices where

import Data.Word

newtype LeafInd = LeafInd {getLeafInd :: Word}
newtype RootInd = RootInd {getRootInd :: Word}
newtype NetworkInd = NetworkInd {getNetworkInd :: Word}
newtype InternalInd = InternalInd {getInternalInd :: Word}
