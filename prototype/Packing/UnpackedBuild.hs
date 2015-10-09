module Packing.UnpackedBuild (performBuild) where

import Packing.PackedBuild
import qualified Data.Map as M
import qualified Packing.BitPackedNode as BN
import qualified Data.Bits as B
import Component
import ReadFiles
import qualified Data.Vector as V
import Data.List
import Data.Word
import Data.Maybe
import Debug.Trace

-- | Master function to determine the mode and do all of the packing after checking for incorrect data types
performBuild :: RawData -> [String] -> PhyloForest -> (PackedForest, PackedInfo, BN.PackMode)
performBuild raw@(pairedData, infoList) names forest
    | charType (head infoList) == Add || charType (head infoList) == Matrix = error "bit packed optimization only handles non-additive characters"
    | otherwise = 
        let 
            numChars = length $ snd $ head pairedData
            special = getSpecial (head infoList)
            overallAlph = special M.! '?'
            pMode = getPackMode overallAlph
            pInfo = getPackInfo raw pMode special
            leaves = createPackedLeaves pMode pInfo raw
            packForest = V.fromList $ map (\tree -> packTree names leaves tree) forest
        in --trace ("packed forest "++ show packForest)
            (packForest, pInfo, pMode)

-- | Function to correctly map the question mark to all characters
-- uses the charInfo as meaningful, but has a fall through in case
getSpecial :: CharInfo -> M.Map Char String
getSpecial charInfo =
    let 
        alph = map head (alphabet charInfo)
        finalAlph = [x | x<-alph, not $ x `elem` "?"]
    in M.fromList [('?', finalAlph)]

-- | Helper function to take string input and output the BN.PackMode object, automatically determining as needed
getPackMode :: [Char] -> BN.PackMode
getPackMode alph
    | alphlen <= 16 = BN.MakePackMode {BN.bitLen = 16, BN.adaptive = False}
    | alphlen <= 64 = BN.MakePackMode {BN.bitLen = 64, BN.adaptive = False}
    | otherwise = BN.MakePackMode {BN.bitLen = 0, BN.adaptive = False}
        where alphlen = length alph

-- | Creates the pack info structure, filling in everything for the word types (in which case it won't be evaluated because of laziness)
-- and only giving the infinite what's meaningful
getPackInfo :: RawData -> BN.PackMode -> SpecialMap -> PackedInfo
getPackInfo (pairedData, _) pMode special = 
        let 
            m = genUnpackedMasks  alphlen numChars pMode
            numChars = length $ snd $ head pairedData
            overallAlph = special M.! '?'
            alphlen = length overallAlph
        in PackedInfo {
                        blockLenMap = V.empty, 
                        bitAlphs = V.empty,
                        shuffleChars = V.empty,
                        specialMap = special,
                        maxAlphabet = overallAlph,
                        totalChars = numChars,
                        blockChars = V.empty,
                        masks = m} 

-- | Creates a leaf matrix for a packed forest given the raw data, simply a recursion of charSetToPacked
createPackedLeaves :: BN.PackMode -> PackedInfo -> RawData -> PackedTree 
createPackedLeaves pMode pInfo (pairedData, infoList) 
    | (length pairedData == 0) = V.empty
    | otherwise = (charSetToPacked (snd $ head pairedData) (head infoList) pInfo pMode) `V.cons` (createPackedLeaves pMode pInfo ((tail pairedData), infoList))


-- | Unified function to get both the occupancy and standard masks
genUnpackedMasks :: Int -> Int -> BN.PackMode -> (BN.BitPackedNode, BN.BitPackedNode)
genUnpackedMasks alphLen numChars mode 
    | (BN.bitLen mode == 16) = 
        let 
            occMask = BN.S16 $ V.replicate numChars (65535 :: Word16)
            sMask = BN.S16 $ V.replicate numChars (B.setBit (0 :: Word16) alphLen)
        in (occMask, sMask)
    | (BN.bitLen mode == 64) = 
        let
            occMask = BN.S64 $ V.replicate numChars (18446744073709551615 :: Word64)
            sMask = BN.S64 $ V.replicate numChars (B.setBit (0 :: Word64) alphLen)
        in (occMask, sMask)
    | otherwise = BN.genMasks V.empty V.empty alphLen numChars mode


-- | Converts a single node into a packed node
charSetToPacked :: [String] -> CharInfo -> PackedInfo -> BN.PackMode -> BN.BitPackedNode
--charSetToPacked strings charInfo pInfo pMode | trace ("charSetToPacked " ++ show strings ++ "with alph " ++ show (maxAlphabet pInfo)) False = undefined
charSetToPacked strings charInfo pInfo pMode
    | ((length strings) == 0) || ((head strings) == "no_data") = BN.EmptyPackNode
    | charType charInfo == Add = error "packing only available for non-additive characters"
    | (length strings /= totalChars pInfo) = error "inconsistent number of characters, unsuitable for bit packing"
    | (BN.bitLen pMode == 16) = 
        let filled = V.fromList $ map (\char -> B.setBit (0 :: Word16) (fromJust $ elemIndex char alph)) strToChar
        in BN.S16 filled
    | (BN.bitLen pMode == 64) = 
        let filled = V.fromList $ map (\char -> B.setBit (0 :: Word64) (fromJust $ elemIndex char alph)) strToChar
        in BN.S64 filled
    | otherwise = BN.makeNode strToChar (specialMap pInfo) (bitAlphs pInfo) (maxAlphabet pInfo) (shuffleChars pInfo) pMode
        where
            strToChar = map (\str -> head str) strings
            alph = maxAlphabet pInfo


-- | Uses a tree traversal to order the packed nodes into a packed tree, 
-- filling in empties elsewhere for a full structure with correct indices
packTree :: [String] -> PackedTree -> PhyloComponent -> PackedTree
--packTree names _ allNodes | trace ("pack tree with names " ++ show names) False = undefined
packTree names leaves allNodes
    | (V.length allNodes == 0 ) = V.empty
    | not amLeaf = BN.EmptyPackNode `V.cons` packTree names leaves (V.tail allNodes)
    | otherwise = 
        let 
            myName = nodeName $ V.head allNodes
            myPos = fromJust $ myName `elemIndex` names
            myNode = leaves V.! myPos
        in myNode `V.cons` packTree names leaves (V.tail allNodes)  
        where 
            amLeaf = isTerminal $ V.head allNodes
