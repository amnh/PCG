module Data.Tree.Edge.SecondReference where

import Data.Set
import Data.Map

type FromConnect = Set Int
type ToConnect = Map Int EdgeInfo
type EdgeReference = (FromConnect, ToConnect)
data EdgeInfo n = EdgeInfo {len :: Float, origin :: n, terminal :: n, connection :: (n, n)}

instance StandardEdge EdgeInfo where
    edgeLen = len
    setEdgeLen e val = e {len = val}
    origin = origin
    terminal = terminal