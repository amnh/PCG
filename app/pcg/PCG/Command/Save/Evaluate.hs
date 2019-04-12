{-# LANGUAGE ScopedTypeVariables #-}
module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Binary            (encodeFile)
import Data.Compact           (getCompact)
import Data.Compact.Serialize (writeCompact)
import Data.Functor           (($>))
import Data.MonoTraversable
import PCG.Command.Save


evaluate :: SaveCommand -> GraphState -> SearchState
evaluate (SaveCommand fileSource serial) g =
  let path = otoList fileSource
  in  case serial of
        Compact -> liftIO $ writeCompact path g $> g
        Binary  -> let graph  = getCompact g
                       refDAG = extractReferenceDAG graph
                   in liftIO $ encodeFile path refDAG $> g



