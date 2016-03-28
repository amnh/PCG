{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph
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
{--}
--evaluate _ old | trace ("evaluate search state for output " ++ show old) False = undefined
evaluate (REPORT target format) old = do
    stateValue <- old
    case generateOutput stateValue format of
     Left  errMsg -> fail errMsg
     Right output ->
       case target of
         OutputToStdout -> old <> info output
         OutputToFile f -> old <* liftIO (writeFile f output)

evaluate _ _ = fail "Invalid READ command binding"
{--}

generateOutput :: Graph -> OutputFormat -> Either String String
-- Don't ignore names later
generateOutput g CrossReferences {} = Right $ taxonReferenceOutput g mempty
generateOutput g Data            {} = Right $ newickReport g
generateOutput g DotFile         {} = Right $ dotOutput g
generateOutput g Metadata        {} = Right $ metadataCsvOutput g
generateOutput _ _ = Left "Unrecognized 'report' command"
