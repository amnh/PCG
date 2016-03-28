{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph
import           Control.Monad.IO.Class
import           Control.Evaluation
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Report.TaxonMatrix
import           PCG.Command.Types.Report.Internal

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
         OutputToFile f -> do
                             !_ <- liftIO $ writeFile f output
                             old

evaluate _ _ = fail "Invalid READ command binding"
{--}

generateOutput :: Graph -> OutputFormat -> Either String String
-- Don't ignore names later
--generateOutput g _ | trace ("generate output on g " ++ show g) False = undefined
generateOutput g (CrossReferences _) = Right $ taxonReferenceOutput g mempty
generateOutput _ _ = Left "Unrecognized 'report' command"
