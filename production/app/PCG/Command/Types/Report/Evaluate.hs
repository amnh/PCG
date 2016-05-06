{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Analysis.ImpliedAlignment.Standard
import           Analysis.Parsimony.Binary.Optimization
import           Bio.PhyloGraph.Solution
import           Bio.PhyloGraph.Tree.Binary.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.List.NonEmpty
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Report.TaxonMatrix
import           PCG.Command.Types.Report.GraphViz
import           PCG.Command.Types.Report.Internal
import           PCG.Command.Types.Report.Metadata
import           PCG.Command.Types.Report.Newick
import           PCG.Command.Types.Report.ImpliedAlignmentFasta

evaluate :: Command -> SearchState -> SearchState
evaluate (REPORT target format) old = do
    stateValue <- old
    case generateOutput stateValue format of
     ErrorCase    errMsg  -> fail errMsg
     MultiStream  streams -> old <* sequenceA (liftIO . uncurry writeFile <$> streams)
     SingleStream output  ->
       case target of
         OutputToStdout -> old <* info output
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
generateOutput :: StandardSolution -> OutputFormat -> FileStreamContext
generateOutput g (CrossReferences fileNames)   = SingleStream $ taxonReferenceOutput g fileNames
generateOutput g Data                       {} = SingleStream . newickReport $ addOptimization g
generateOutput g DotFile                    {} = SingleStream $ dotOutput g
generateOutput g Metadata                   {} = SingleStream $ metadataCsvOutput g
generateOutput g ImpliedAlignmentCharacters {} = let g' = addOptimization g
                                                 in case iaOutput (iaSolution g') g' of
                                                   [] -> ErrorCase "There were no Dynamic homology characters on which to perform an implied alignment."
                                                   xs -> MultiStream $ fromList xs 
generateOutput _ _ = ErrorCase "Unrecognized 'report' command"

type FileContent = String

data FileStreamContext
   = ErrorCase    String
   | SingleStream FileContent
   | MultiStream  (NonEmpty (FilePath,FileContent))
