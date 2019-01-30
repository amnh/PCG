-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.SequentialAlign
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module exposing a sequential alignment optimization.
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Analysis.Parsimony.Dynamic.SequentialAlign
  ( sequentialAlign
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (handleMissingCharacter)
import qualified Analysis.Parsimony.Dynamic.SequentialAlign.FFI                  as FFI
import           Bio.Character.Encodable
import           Data.TCM.Memoized
import           FFI.Character.Exportable.Class


-- |
-- 'sequentialAlign' is similar to DO, but uses Yu's and Vahid's information-theoretic sequential alignment
-- algorithm to produce the alignment
sequentialAlign :: (EncodableDynamicCharacter s, Exportable s) => MemoizedCostMatrix -> s -> s -> (Word, s, s, s, s)
sequentialAlign matrix lhs rhs = handleMissingCharacter lhs rhs $ FFI.pairwiseSequentialAlignment matrix lhs rhs
