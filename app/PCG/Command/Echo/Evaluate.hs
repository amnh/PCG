{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Echo.Evaluate
  ( evaluate
  ) where

import           Analysis.Parsimony.Binary.Optimization
import           Bio.Phylogeny.Solution
import           Bio.Phylogeny.Tree.Binary.Class
import           Control.Monad.IO.Class
import           Control.Evaluation
import           PCG.Command.Types (Command(..))

-- TODO: define the ECHO command parser and evaluation.
evaluate :: Command -> SearchState -> SearchState
evaluate (REPORT target format) old = undefined
evaluate _ _ = fail "Invalid ECHO command binding"
