-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization export of Ukkonen's space & time saving algorithm.
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen
  ( ukkonenDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Internal
