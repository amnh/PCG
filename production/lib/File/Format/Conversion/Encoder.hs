-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Conversion.Encoder
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common encoding functionality to many of the converters
--
-----------------------------------------------------------------------------

module File.Format.Conversion.Encoder where

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.PhyloCharacter
import           Bio.Sequence.Coded
import           Bio.Sequence.Parsed
import           Control.Monad
import           Data.BitVector        hiding (foldr, replicate, join, concat, not)
import           Data.Int
import           Data.List             hiding (zipWith)
import qualified Data.Map.Lazy         as M
import           Data.Maybe
import           Data.Matrix.NotStupid              (matrix)
import           Data.Vector                  (Vector, ifoldr, zipWith, cons)
import qualified Data.Vector           as V   
import           Prelude               hiding (zipWith)

import Debug.Trace

type Encoded = EncodedSeq BitVector

dnaAlph, rnaAlph, aaAlph :: [String]
dnaAlph = ["A", "C", "G", "T", "-"] 
rnaAlph = ["A", "C", "G", "U", "-"]
aaAlph = ["R", "H", "K", "D", "E", "S", "T", "N", "Q", "C", "U", "G", "P", "A", "V", "L", "I", "M", "F", "Y", "W", "-"]

-- | Functionality to make char info from tree seqs
makeEncodeInfo :: TreeSeqs -> Vector CharInfo
makeEncodeInfo seqs = --trace ("makeEncodeInfo " ++ show alphabets)
                        zipWith makeOneInfo alphabets allChecks
    where
        alphabets = developAlphabets seqs
        allChecks = checkAlignLens seqs

-- | Internal function to create alphabets
developAlphabets :: TreeSeqs -> Vector Alphabet
developAlphabets inSeqs = V.map setGapChar $ V.map sort $ M.foldr (zipWith getNodeAlphAt) initializer inSeqs
    where
        someSeq = head $ M.elems inSeqs
        initializer = if null someSeq then mempty
                        else V.replicate (V.length someSeq) mempty

        getNodeAlphAt :: Maybe ParsedSeq -> Alphabet -> Alphabet
        --getNodeAlphAt inSeq soFar | trace ("getNodeAlphAt " ++ show inSeq ++ " with accum " ++ show soFar) False = undefined
        getNodeAlphAt inSeq soFar
            | isNothing inSeq = mempty
            | otherwise =  V.foldr (flip $ foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev)) soFar (fromJust inSeq)

        -- | Ensure that the gap char is present and correctly positioned in an alphabet
        setGapChar :: Alphabet -> Alphabet
        setGapChar inAlph = case elemIndex "-" inAlph of
            Just i -> take i inAlph ++ drop i inAlph ++ ["-"]
            Nothing -> inAlph ++ ["-"]

-- | Internal function to make one character info
makeOneInfo :: Alphabet -> (Bool, Int) -> CharInfo
makeOneInfo inAlph (isAligned, seqLen)
    | inAlph `subsetOf` dnaAlph = DNA "" isAligned masks mempty inAlph defaultMat False
    | inAlph `subsetOf` rnaAlph = RNA "" isAligned masks mempty inAlph defaultMat False
    | inAlph `subsetOf` aaAlph = AminoAcid "" isAligned masks inAlph mempty defaultMat False
    | otherwise = Custom "" isAligned masks inAlph mempty defaultMat False False
        where 
            defaultMat = matrix (length inAlph) (length inAlph) (const 1)
            masks = generateMasks (length inAlph) seqLen isAligned

            generateMasks :: Int -> Int -> Bool -> (Encoded, Encoded)
            generateMasks alphLen sLen isAligned 
                | isAligned = 
                    let 
                        periodic = fromBits $ concat (replicate sLen unit)
                        occupancy = fromBits $ replicate (alphLen * sLen) True
                    in (Just $ V.singleton occupancy, Just $ V.singleton periodic)
                | otherwise = 
                    let
                        periodic = fromBits <$> replicate sLen unit
                        occupancy = fromBits <$> replicate sLen (replicate alphLen True)
                    in (Just $ V.fromList occupancy, Just $ V.fromList periodic)
                    where
                        unit = replicate (alphLen - 1) False ++ [True]


checkAlignLens :: TreeSeqs -> Vector (Bool, Int)
checkAlignLens = M.foldr matchLens mempty
    where
        matchLens :: ParsedSequences -> Vector (Bool, Int) -> Vector (Bool, Int)
        matchLens curSeqs prevVals
            | V.null prevVals = V.map makeVal curSeqs
            | otherwise = V.zipWith checkVal curSeqs prevVals
                where
                    checkVal v pv = if isNothing v then (False, 0)
                                        else (V.length (fromJust v) == snd pv, maximum [V.length $ fromJust v, snd pv])
                    makeVal v = if isNothing v then (False, 0)
                                    else (True, V.length $ fromJust v)

subsetOf :: (Ord a) => [a] -> [a] -> Bool
subsetOf list1 list2 = foldr (\e acc -> acc && e `elem` list2) True list1

encodeIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
encodeIt = zipWith (\s info -> join $ flip encodeOverMetadata info <$> s)

packIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
packIt = encodeIt

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf n xs 
    | null xs = mempty
    | otherwise = f `cons` chunksOf n s
      where
        (f,s) = V.splitAt n xs

-- | Function to encode into minimal bits
encodeMinimal :: (Bits b, Num b, Show b) => ParsedSeq -> Alphabet -> EncodedSeq b
--encodeMinimal strSeq alphabet | trace ("encodeMinimal over alphabet " ++ show alphabet ++ " of seq " ++ show strSeq) False = undefined
encodeMinimal strSeq alphabet = 
    let 
        z = zeroBits
        bigBit = shift (bit (alphLen * V.length strSeq - 1)) 1
        alphLen = length alphabet
        foldAmbig = foldr (\c acc -> setSingleElem c acc alphabet)
        groupEncode = ifoldr (\i ambig acc -> shift (foldAmbig z ambig) (i * alphLen) .|. acc) z
        coded = case bitSizeMaybe z of
                Nothing -> V.singleton $ ifoldr (\i ambig acc -> shift (foldAmbig bigBit ambig) (i * alphLen) .|. acc) bigBit strSeq
                Just width -> let groupParsed = chunksOf (width `div` alphLen) strSeq
                              in V.map groupEncode groupParsed
    in if null coded then Nothing else Just coded

-- | Function to encode over maximal bits
encodeMaximal :: Bits b => ParsedSeq -> Alphabet -> EncodedSeq b
encodeMaximal strSeq alphabet = 
    let coded = V.map (foldr (\c acc -> setSingleElem c acc alphabet) zeroBits) strSeq
    in if null coded then Nothing else Just coded

-- | Function to encode given metadata information
encodeOverMetadata :: (Bits b, Num b, Show b) => ParsedSeq -> PhyloCharacter (EncodedSeq b) -> EncodedSeq b
encodeOverMetadata strSeq metadata = case metadata of
    DNA         _ align _ _ _ _ _       -> if align then minEncode else maxEncode
    RNA         _ align _ _ _ _ _       -> if align then minEncode else maxEncode
    Qualitative _ align _ _ _ _ add _   -> if align && not add then minEncode else maxEncode
    AminoAcid   _ align _ _ _ _ _       -> if align then minEncode else maxEncode
    Custom      _ align _ _ _ _ _ add   -> if align && not add then minEncode else maxEncode
    _                                   -> maxEncode
    where
        minEncode = encodeMinimal strSeq (alphabet metadata)
        maxEncode = encodeMaximal strSeq (alphabet metadata)

-- Function to set a single element
setSingleElem :: Bits b => String -> b -> Alphabet -> b
setSingleElem char orig alphabet = case elemIndex char alphabet of
    Nothing -> orig
    Just pos -> setBit orig pos  

