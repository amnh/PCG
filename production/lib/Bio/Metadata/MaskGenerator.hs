-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.MaskGenerator
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Adds Fitch masks to existing metadata on a graph
--
-----------------------------------------------------------------------------

module Bio.Metadata.MaskGenerator where

import Bio.Metadata
import Bio.PhyloGraph.Solution
import Bio.Character.Dynamic
import Data.Alphabet
import Data.Foldable
import Data.HashMap.Strict        (elems)
import Data.Key
import Data.Maybe
import qualified Data.Vector as V

-- | Mutate a 'StandardSolution' to include masks in the metadata structure
addMasks :: StandardSolution -> StandardSolution
addMasks inSolution = inSolution { metadata = V.imap changeMetadata (metadata inSolution) }
    where
        changeMetadata :: Int -> StandardMetadata -> StandardMetadata
        changeMetadata pos curChar
            | isAligned curChar = curChar {fitchMasks = generateMasks (alphabet curChar) (getSeqLen pos)}
            | otherwise = curChar

        -- | Get length of a sample sequence, operating under assumption they're all the same
        getSeqLen :: Int -> Int
        getSeqLen pos = length $ fromMaybe mempty curSeq
            where
                someSeqs = head . elems $ parsedChars inSolution
                curSeq   = someSeqs V.! pos

        -- | Generate mask pair given proper info
       {- generateMasks :: Int -> Int -> (DynamicChar, DynamicChar)
        generateMasks alphLen sLen = (DynamicChar alphLen occupancy gapChar, DynamicChar alphLen periodic gapChar)
            where
                unit      = replicate (alphLen - 1) False <> [True]
                periodic  = fromBits $ concat (replicate sLen unit)
                occupancy = fromBits $ replicate (alphLen * sLen) True
                gapChar   = (bitVec alphLen (0 :: Integer)) (alphLen - 1)
        -}
-- | Generate mask pair given proper info
generateMasks :: Alphabet String -> Int -> (DynamicChar, DynamicChar)
generateMasks inAlphabet sLen = --trace ("encode masks " ++ show periodic) $
                                (encodeDynamic inAlphabet occupancy, encodeDynamic inAlphabet periodic)
    where
        unit      = [inAlphabet ! (length inAlphabet - 1)]
        periodic  = V.replicate sLen unit
        occupancy = V.replicate sLen (toList inAlphabet)



