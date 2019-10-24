-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Build
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"BUILD\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnboxedSums        #-}



module PCG.Command.Build
  ( BuildCommand(..)
  , ConstructionType(..)
  , ClusterLabel(..)
  , ClusterOption(..)
  , ClusterSplit(..)
  , buildCommandSpecification
  , clusterSplit
  ) where


import Control.Applicative.Free (Ap)
import Data.Functor             (($>))
import PCG.Syntax.Combinators


-- |
-- The \"BUILD\" command specifying how a component graph should be constructed.
-- output should be directed.
data  BuildCommand
    = BuildCommand {-# UNPACK #-} !Int !ConstructionType !ClusterOption
    deriving stock (Show)

-- |
-- Different possible types of component graph construction.
data  ConstructionType
    = WagnerTree
    | WheelerNetwork
    | WheelerForest
    deriving stock (Eq, Show)


-- |
-- Different possible types of clustering pre-pass.
data  ClusterLabel
    = NoCluster
    | SingleLinkage
    | CompleteLinkage
    | UPGMALinkage
    | WeightedLinkage
    | WardLinkage
    | KMedians
    deriving stock (Eq, Show)

-- |
-- Options on how to divide a clustering
data ClusterSplit
  = ClusterGroup Int
  | ClusterCut   Double
  deriving stock (Eq, Show)


-- |
-- A clustering specification with type and grouping.
data ClusterOption = ClusterOption !ClusterLabel !ClusterSplit
    deriving stock (Eq, Show)


-- |
-- Get the number of clusters from a 'ClusterOption'.
clusterSplit :: ClusterOption -> ClusterSplit
clusterSplit (ClusterOption _ s) = s

-- |
-- Defines the semantics of interpreting a valid \"BUILD\" command from the PCG
-- scripting language syntax.
buildCommandSpecification :: CommandSpecification BuildCommand
buildCommandSpecification = command "build" . argList $
  BuildCommand <$> trajectoryCount <*> constructionType <*> clusterOptionType


trajectoryCount :: Ap SyntacticArgument Int
trajectoryCount = int `withDefault` 1


constructionType :: Ap SyntacticArgument ConstructionType
constructionType = choiceFrom [ buildTree, buildNetwork, buildForest ] `withDefault` WagnerTree
  where
    buildTree    = value "tree"    $> WagnerTree
    buildNetwork = value "network" $> WheelerNetwork
    buildForest  = value "forest"  $> WheelerForest


clusterOptionType :: Ap SyntacticArgument ClusterOption
clusterOptionType =
  (argId "cluster" . argList $ ClusterOption <$> clusterLabelType <*> clusterSplitType)
  `withDefault` ClusterOption NoCluster (ClusterGroup 1)

clusterLabelType :: Ap SyntacticArgument ClusterLabel
clusterLabelType =
    choiceFrom
      [ noCluster
      , singleLinkage
      , completeLinkage
      , upgmaLinkage
      , weightedLinkage
      , wardLinkage
      , kMedians
      ]
      `withDefault` NoCluster
  where
    noCluster       = value "no-cluster" $> NoCluster
    singleLinkage   = value "single "    $> SingleLinkage
    completeLinkage = value "complete"   $> CompleteLinkage
    upgmaLinkage    = value "upgma"      $> UPGMALinkage
    weightedLinkage = value "weighted"   $> WeightedLinkage
    wardLinkage     = value "ward"       $> WardLinkage
    kMedians        = value "k-medians"  $> KMedians


clusterSplitType :: Ap SyntacticArgument ClusterSplit
clusterSplitType =
    choiceFrom
      [ groupCluster
      , cutCluster
      ]
      `withDefault`
        ClusterCut 0.7
  where
    groupCluster = argId "group" $ ClusterGroup <$> int
    cutCluster   = argId "cut"   $ ClusterCut   <$> real `withDefault` 0.7

