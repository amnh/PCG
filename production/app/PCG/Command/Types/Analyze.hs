module PCG.Command.Types.Analyze where

import PCG.Command.Types

evaluate :: Command -> SearchState -> SearchState
evaluate (ANALYZE analysis) curState = -- do
evaluate _ _ = fail "Invalid ANALYZE command binding"
