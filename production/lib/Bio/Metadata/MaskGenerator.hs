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
import           Bio.Phylogeny.PhyloCharacter
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
        changeMetadata :: Int -> CharacterMetadata -> CharacterMetadata
        changeMetadata pos curChar 
            | aligned curChar = curChar {fitchMasks = generateMasks (length $ alphabet curChar) (getSeqLen pos)}
            | otherwise = curChar

--addMasks :: Graph -> Graph
--addMasks (Graph dags) = Graph $ map addToDAG dags
--    where
--        addToDAG :: DAG -> DAG
--        addToDAG inDAG = inDAG {characters = imap (\i c -> if aligned c then addToChar i c inDAG else c) (characters inDAG)}

--        addToChar :: Int -> CharInfo -> DAG -> CharInfo
--        addToChar pos char curDAG = char {fitchMasks = generateMasks (length $ alphabet char) (getSeqLen pos curDAG)}

--        -- | Get length of a sample sequence, assume they're all the same
--        getSeqLen :: Int -> DAG -> Int
--        getSeqLen curPos curDAG = V.length $ fromMaybe mempty curSeq 
--            where
--                someTerminal = code $ V.head $ V.filter isLeaf (nodes curDAG)
--                someSeqs = (parsedSeqs curDAG) ! ((nodeNames curDAG) ! someTerminal)
--                curSeq = someSeqs V.! curPos

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





