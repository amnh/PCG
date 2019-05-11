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


evaluate :: LoadCommand -> SearchState
evaluate (LoadCommand filePath) = do
    result <- liftIO . runValidationT $ deserializeCompact filePath
    case result of
      Success gVal -> pure gVal :: SearchState
      Failure eVal ->
        case eVal of
          Left  iErr-> state $ failWithPhase Inputing iErr
          Right pErr-> state $ failWithPhase  Parsing pErr

