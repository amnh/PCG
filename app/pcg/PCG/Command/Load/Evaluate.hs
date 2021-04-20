------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Load.Evaluate
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module PCG.Command.Load.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Evaluation
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Validation
import Data.FileSource.IO
import Data.Validation
import PCG.Command.Load


-- |
-- Evaluates a 'LoadCommand' by reading in a graph state from disk and setting
-- it as the current working state of the process.
evaluate :: LoadCommand -> SearchState
evaluate (LoadCommand filePath) = do
    result <- liftIO . runValidationT $ deserializeBinary filePath
    case result of
      Success gVal -> pure gVal :: SearchState
      Failure eVal ->
        case eVal of
          Left  iErr-> failWithPhase Inputing iErr
          Right pErr-> failWithPhase  Parsing pErr
