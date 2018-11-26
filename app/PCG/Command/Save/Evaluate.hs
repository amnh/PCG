{-# LANGUAGE ScopedTypeVariables #-}
module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Bio.Graph.ReferenceDAG (_dagCost, _graphData)
import Control.Monad.IO.Class (liftIO)
import Data.Binary            (decodeFile, encodeFile)
import Data.Binary.Utility
import Data.ByteString.Lazy   as BS (writeFile)
import Data.Compact           (getCompact)
import Data.Compact.Serialize (writeCompact)
import Data.Functor           (($>))
import PCG.Command.Save


evaluate :: SaveCommand -> GraphState -> SearchState
evaluate (SaveCommand filePath serial) g =
  case serial of
    Compact -> liftIO $ writeCompact filePath g $> g
    Binary  ->
      do
        let graph  = getCompact g
        let refDAG = extractReferenceDAG graph
        liftIO $ encodeFile filePath refDAG
        pure g



