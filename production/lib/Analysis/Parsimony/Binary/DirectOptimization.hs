-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.DirectOptimization 
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
module Analysis.Parsimony.Binary.DirectOptimization 
    ( doAlignment
    , getOverlap
    , minimalChoice
    , naiveDO
    ) where

import Analysis.Parsimony.Binary.DirectOptimization.Internal
