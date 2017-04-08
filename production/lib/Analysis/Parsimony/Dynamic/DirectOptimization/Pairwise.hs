-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization export
--
-----------------------------------------------------------------------------
module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
    ( doAlignment
    , filterGaps
    , getOverlap
    , minimalChoice
    , naiveDO
    , naiveDOConst
    , naiveDOMemo
    ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
