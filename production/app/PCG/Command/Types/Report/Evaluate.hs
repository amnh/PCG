{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Analysis.Parsimony.Binary.Optimization
import           Bio.PhyloGraph.Solution
import           Bio.PhyloGraph.Tree.Binary.Class
import           Control.Monad.IO.Class
import           Control.Evaluation
import           Data.Monoid ((<>))
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Report.TaxonMatrix
import           PCG.Command.Types.Report.GraphViz
import           PCG.Command.Types.Report.Internal
import           PCG.Command.Types.Report.Metadata
import           PCG.Command.Types.Report.Newick

evaluate :: Command -> SearchState -> SearchState
evaluate (REPORT target format) old = do
    stateValue <- old
    case generateOutput stateValue format of
     Left  errMsg -> fail errMsg
     Right output ->
       case target of
         OutputToStdout -> old <> info output
         OutputToFile f -> old <* liftIO (writeFile f output)

evaluate _ _ = fail "Invalid READ command binding"

-- | Function to add optimization to the newick reporting
-- TODO: change this error into a warning
addOptimization :: StandardSolution -> StandardSolution 
addOptimization result
  | allBinary = solutionOptimization 1 result
  | otherwise = error "Cannot perform optimization because graph is not binary, outputting zero cost"
    where allBinary = all (all verifyBinary) (forests result)

-- TODO: Redo reporting
generateOutput :: StandardSolution -> OutputFormat -> Either String String
generateOutput g (CrossReferences fileNames) = Right $ taxonReferenceOutput g fileNames
generateOutput g Data            {}          = Right . newickReport $ addOptimization g
generateOutput g DotFile         {}          = Right $ dotOutput g
generateOutput g Metadata        {}          = Right $ metadataCsvOutput g
generateOutput _ _ = Left "Unrecognized 'report' command"
