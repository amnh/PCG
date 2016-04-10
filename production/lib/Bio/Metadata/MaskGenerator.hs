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

import           Bio.Phylogeny.Solution
import           Bio.Metadata
import           Bio.Sequence.Coded
import           Data.BitVector      (fromBits)
import           Data.HashMap.Strict (elems)
import           Data.Maybe
import           Data.Vector         (imap)
import qualified Data.Vector as V

--type Encoded = EncodedSeq BitVector

addMasks :: StandardSolution -> StandardSolution
addMasks inSolution = inSolution {metadata = imap changeMetadata (metadata inSolution)}
    where
        changeMetadata :: Int -> StandardMetadata -> StandardMetadata
        changeMetadata pos curChar 
            | isAligned curChar = curChar {fitchMasks = generateMasks (length $ alphabet curChar) (getSeqLen pos)}
            | otherwise = curChar

        -- | Get length of a sample sequence, operating under assumption they're all the same
        getSeqLen :: Int -> Int
        getSeqLen pos = V.length $ fromMaybe mempty curSeq
            where
                someSeqs = head . elems $ parsedChars inSolution
                curSeq = someSeqs V.! pos

        -- | Generate mask pair given proper info
        generateMasks :: Int -> Int -> (EncodedSeq, EncodedSeq)
        generateMasks alphLen sLen = (Just occupancy, Just periodic)
            where
                unit = replicate (alphLen - 1) False ++ [True]
                periodic = fromBits $ concat (replicate sLen unit)
                occupancy = fromBits $ replicate (alphLen * sLen) True





