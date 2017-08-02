-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Report.Graphviz
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a graphviz format from a Graph
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Report.GraphViz where

import           Bio.Graph.PhylogeneticDAG
import           Data.GraphViz.Printing hiding ((<>)) -- Seriously, why is this redefined?
import           Data.Semigroup
import qualified Data.Text.Lazy         as L


generateDotFile :: GraphState -> String
generateDotFile = (<> "\n") . L.unpack . renderDot . getDotGraph


getDotGraph :: GraphState -> DotCode
getDotGraph = either toDot toDot

