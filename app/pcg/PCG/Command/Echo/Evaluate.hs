module PCG.Command.Echo.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Functor           (($>))
import Data.Text.IO           (putStrLn)
import Data.Text.Short        (toText)
import PCG.Command.Echo
import Prelude                hiding (putStrLn)


evaluate :: EchoCommand -> GraphState -> SearchState
evaluate (EchoCommand message) g = liftIO $ putStrLn (toText message) $> g
