{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Analysis.Parsimony.Binary.Optimization
import           Bio.Phylogeny.Solution
import           Bio.Phylogeny.Tree.Binary.Class
import           Control.Monad.IO.Class
import           Control.Evaluation
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Report.TaxonMatrix
import           PCG.Command.Types.Report.GraphViz
import           PCG.Command.Types.Report.Internal
import           PCG.Command.Types.Report.Metadata
import           PCG.Command.Types.Report.Newick

import Debug.Trace

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
addOptimization :: StandardSolution -> StandardSolution --Graph -> Graph
addOptimization result
  | allBinary = solutionOptimization 1 result
  | otherwise = error ("Cannot perform optimization because graph is not binary, outputting zero cost") result
    where allBinary = all (all verifyBinary) (forests result)

-- TODO: Redo reporting
generateOutput :: StandardSolution -> OutputFormat -> Either String String
--generateOutput _ f | trace (show f) False = undefined
generateOutput g (CrossReferences fileNames) = Right $ taxonReferenceOutput g fileNames
generateOutput g Data            {}          = let !x = (addOptimization g) in Right $ newickReport x
generateOutput g DotFile         {}          = Right $ dotOutput g
generateOutput g Metadata        {}          = Right $ metadataCsvOutput g
generateOutput _ _ = Left "Unrecognized 'report' command"
