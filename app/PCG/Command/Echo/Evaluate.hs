module PCG.Command.Echo.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Compact           (Compact)
import Data.Functor           (($>))
import PCG.Command.Echo


evaluate :: EchoCommand -> GraphState -> SearchState
evaluate (EchoCommand message) g = liftIO (putStrLn message) $> g
