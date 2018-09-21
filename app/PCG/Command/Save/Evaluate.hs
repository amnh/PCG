module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Compact.Serialize (writeCompact)
import Data.Functor           (($>))
import PCG.Command.Save
import PCG.Syntax

evaluate :: Command -> GraphState -> SearchState
evaluate (SAVE (SaveCommand filePath)) g = liftIO (writeCompact filePath g) $> g

evaluate _ _                             = error "Invalid SAVE command."
