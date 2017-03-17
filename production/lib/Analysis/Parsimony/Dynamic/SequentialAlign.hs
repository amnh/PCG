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
-- Module exposing an alignment optimization from Yu Xiang's research at Harvard.
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Dynamic.SequentialAlign
  ( generateMemoizedCostMatrix
  , sequentialAlign
  ) where

import qualified Analysis.Parsimony.Dynamic.SequentialAlign.FFI as FFI
import           Bio.Character.Encodable
import           Bio.Character.Exportable.Class


-- |
-- sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
sequentialAlign :: (EncodableDynamicCharacter s, Exportable s, Show s) => FFI.MemoizedCostMatrix -> s -> s -> (s, Double, s, s, s)
sequentialAlign = FFI.pairwiseSequentialAlignment

-- TODO: put this in Bio.Metadata probably
generateMemoizedCostMatrix :: Word -> (Word -> Word -> Word) -> FFI.MemoizedCostMatrix
generateMemoizedCostMatrix = FFI.getMemoizedCostMatrix
