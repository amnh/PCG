-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.CharacterMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a matrix stating which characters are present in which files.
-- Most importantly, it assumes that all character names have the file name prepended to them.
--
-----------------------------------------------------------------------------

module PCG.Command.Types.Report.CharacterMatrix where

import Bio.Phylogeny.Graph

-- | Wrapper function to put a graph in a character matrix
outPutMatrix :: String -> Graph -> IO ()
outPutNewick fileName inGraph = writeFile fileName (toCharMat inGraph)
    where
        toCharMat :: Graph -> String
        toCharMat inGraph = foldr (combineMats . makeMat)