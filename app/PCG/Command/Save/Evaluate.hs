module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Compact.Serialize (writeCompact)
import Data.Functor           (($>))
import PCG.Command.Save
import PCG.Syntax

evaluate :: SaveCommand -> GraphState -> SearchState
evaluate (SaveCommand filePath) g = liftIO (writeCompact filePath g) $> g
