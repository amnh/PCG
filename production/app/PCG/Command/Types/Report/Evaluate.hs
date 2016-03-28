{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph
import           Control.Monad.IO.Class
import           Control.Evaluation
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Report.CharacterMatrix
import           PCG.Command.Types.Report.Internal

evaluate :: Command -> SearchState -> SearchState
{--}
evaluate (REPORT target format) old = do
    stateValue <- old
    case generateOutput stateValue format of
     Left  errMsg -> fail errMsg
     Right output ->
       case target of
         OutputToStdout -> old <> info output
         OutputToFile f -> stateValue <$ (liftIO $ writeFile f output)

evaluate _ _ = fail "Invalid READ command binding"
{--}

generateOutput :: Graph -> OutputFormat -> Either String String
-- Don't ignore names later
generateOutput g (CrossReferences _) = Right $ crossReferenceOutput g
generateOutput _ _ = Left "Unrecognized 'report' command"
