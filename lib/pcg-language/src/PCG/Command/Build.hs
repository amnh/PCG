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

{-# LANGUAGE UnboxedSums #-}


module PCG.Command.Build
  ( BuildCommand(..)
  , ConstructionType(..)
  , ClusterLabel(..)
  , ClusterOption(..)
  , buildCommandSpecification
  ) where


import Control.Applicative.Free (Ap)
import Data.Functor             (($>))
import PCG.Syntax.Combinators


-- |
-- The \"BUILD\" command specifying how a component graph should be constructed.
-- output should be directed.
data  BuildCommand
    = BuildCommand {-# UNPACK #-} !Int !ConstructionType !ClusterOption
    deriving (Show)

-- |
-- Different possible types of component graph construction.
data  ConstructionType
    = WagnerTree
    | WheelerNetwork
    | WheelerForest
    deriving (Eq, Show)

-- |
-- Different possible types of clustering pre-pass.
data ClusterLabel
    = NoCluster      
    | SingleLinkage  
    | CompleteLinkage
    | UPGMALinkage   
    | WeightedLinkage
    | WardLinkage
    | KMedians
  deriving Show

data ClusterOption = ClusterOption !ClusterLabel !Int
  deriving Show
    
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
  value "cluster" *> (argList $ ClusterOption <$> clusterLabelType <*> int)
  `withDefault` (ClusterOption NoCluster 1)

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
    noCluster       = value "no-cluster"       $> NoCluster 
    singleLinkage   = value "single-linkage "  $> SingleLinkage 
    completeLinkage = value "complete-linkage" $> CompleteLinkage
    upgmaLinkage    = value "upgma-linkage"    $> UPGMALinkage
    weightedLinkage = value "weighted-linkage" $> WeightedLinkage
    wardLinkage     = value "ward-linkage"     $> WardLinkage
    kMedians        = value "k-medians"        $> KMedians
