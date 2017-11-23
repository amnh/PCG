module PCG.Command.Types.Analyze where

import PCG.Command.Types

evaluate :: Command -> SearchState -> SearchState
evaluate (ANALYZE analysis) curState = fail "Analyize command not yet implemented"
evaluate _ _ = fail "Invalid ANALYZE command binding"
