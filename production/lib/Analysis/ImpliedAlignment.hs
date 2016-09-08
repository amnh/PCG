-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

-- TODO: Make an ImpliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment
  ( iaSolution
  ) where

import Analysis.ImpliedAlignment.DynamicProgramming
{-
import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Analysis.ImpliedAlignment.Internal
import           Analysis.Parsimony.Binary.DirectOptimization
import           Bio.Metadata
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node     hiding  (Node,children, name)
import           Bio.PhyloGraph.Solution
import           Bio.Character.Dynamic.Coded
import           Control.Arrow                   ((&&&))
import           Data.Foldable
import qualified Data.HashMap.Lazy       as HM
import qualified Data.IntMap             as IM
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet             as IS
import           Data.Key
import           Data.List                       (sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Ord                        (comparing)
import qualified Data.Sequence           as Seq
import           Data.Vector                     (Vector)
import qualified Data.Vector             as V
import           Data.Vector.Instances           ()
import           Prelude                 hiding  (lookup,zip,zipWith)
import           Safe                            (tailMay)
-}

