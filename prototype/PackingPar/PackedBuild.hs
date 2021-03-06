{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module PackingPar.PackedBuild (performPack, 
                        PackedInfo (blockLenMap, bitAlphs, shuffleChars, specialMap, maxAlphabet, totalChars, blockChars, masks), 
                        PackedTree, 
                        PackedForest) where

import qualified Data.Vector as V
import ReadFiles
import Component
import Debug.Trace
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Control.Parallel.Strategies
import PackingPar.ParEnabledNode2
import Data.Bits
import qualified Data.Array.Accelerate as A
import GHC.Generics

-- | Define necessary types, mainly useful structures of nodes
type SeenAlphs = [[Char]]
type PackedTree a = V.Vector (ParEnabledNode a)
type PackedForest a = V.Vector (PackedTree a)
type Masks a = (ParEnabledNode a, ParEnabledNode a)
type BlockLenMap = V.Vector Int
type BitAlphabets = V.Vector (V.Vector [Char])
type ShuffleChars = V.Vector [Int]
type SpecialMap = M.Map Char String
    
instance (NFData a) => NFData (PackedInfo a)

-- | Data types for info on the pack.  Note that for static packing not all PackedInfo fields will be useful or filled
data PackedInfo a = PackedInfo     { blockLenMap :: BlockLenMap
                                , bitAlphs :: BitAlphabets
                                , shuffleChars :: ShuffleChars
                                , specialMap :: SpecialMap
                                , maxAlphabet :: [Char]
                                , totalChars :: Int
                                , blockChars :: V.Vector Int
                                , masks :: Masks a
                                } deriving (Show, Eq, Generic)


---- | Standard special characters for genetic and amino acid sequences, based on standard codes
--specialCharsNuc, specialCharsGene :: SpecialMap
--specialCharsGene = M.fromList [('?', "ACGT-"), ('R', "AG"), ('Y', "CT"), ('S', "GC"), ('W', "AT"), ('K', "GT"), ('M', "AC"), ('B', "CGT"), 
--                    ('D', "AGT"), ('H', "ACT"), ('H', "ACT"), ('V', "ACG"), ('N', "ACGT")] 
--specialCharsNuc = M.fromList [('?', "ACDEFGHIKLMNPQRSTVWY")]

-- | Master function to determine the mode and do all of the packing after checking for incorrect data types
performPack :: (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => a -> RawData -> [String] -> PhyloForest -> (String, String) -> ParMode -> (PackedForest a, PackedInfo a, ParPackMode)
performPack dummy raw@(pairedData, infoList) names forest inMode parMode
    | charType (head infoList) == Add || charType (head infoList) == Matrix = error "bit packed optimization only handles non-additive characters"
    | otherwise = 
        let 
            numChars = length $ snd $ head pairedData
            alphabets = --trace ("got alphabets " ++ show numChars)
                        genSeenAlphabets pairedData numChars
            special = --trace ("alphabets " ++ show (length alphabets))
                        getSpecial alphabets (head infoList)
            overallAlph = --trace ("special " ++ show special)
                            special M.! '?'
            pMode = getPackMode inMode overallAlph
            pInfo = --trace ("mode " ++ show pMode)
                        getPackInfo dummy raw pMode alphabets special parMode
            leaves = --trace ("info " ++ show pInfo)
                        createPackedLeaves dummy pMode pInfo raw parMode
            packForest = --trace ("leaves " ++ show leaves)
                        V.fromList $ map (\tree -> packTree names leaves tree) forest
        in --trace ("packed forest " ++ show packForest)
                (packForest, pInfo, pMode)


-- | Helper function to take string input and output the BN.PackMode object, automatically determining as needed
getPackMode :: (String, String) -> [Char] -> ParPackMode
--getPackMode (genMode, bitLen) alph | trace ("getPackMode " ++ show alph) False = undefined
getPackMode (genMode, bitLen) alph
    | genMode == "automatic" = setAutoPack alphlen
    | bitLen == "16" && alphlen > 16 = trace ("bit length set to 16 but alphabet length is " ++ show alphlen ++ " auto packing instead")
                                        setAutoPack alphlen
    | bitLen == "64" && alphlen > 64 = trace ("bit length set to 64 but alphabet length is " ++ show alphlen ++ " auto packing instead")
                                        setAutoPack alphlen
    | genMode == "adaptive" && bitLen == "16" = A16
    | genMode == "adaptive" && bitLen == "64" = A64
    | genMode == "static" && bitLen == "16" = S16
    | genMode == "static" && bitLen == "64" = S64
    | genMode == "static" && bitLen == "infinite" = error "Long vectors cannot be evaluated in parallel"
    | otherwise = trace ("pack mode set incorrectly, performing auto mode pack") (setAutoPack alphlen) 
        where alphlen = length alph

-- | Helper function to determine the best packing mode for some data
-- based on early results it looks like static 64 performs best, but further use cases could change this
setAutoPack :: Int -> ParPackMode
setAutoPack alphlen 
    | alphlen <= 64 = S64
    | otherwise = error "Alphabet too long, cannot be evaluated in parallel"

-- | Creates the pack info structure, filling in everything for the word types (in which case it won't be evaluated because of laziness)
-- and only giving the infinite what's meaningful
getPackInfo :: (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => a -> RawData -> ParPackMode -> SeenAlphs -> SpecialMap -> ParMode -> PackedInfo a
getPackInfo dummy (pairedData, _) pMode alphabets special parMode
    | pMode == A16 = 
        let 
            (blocklens, finalAlphs, shuffle) = adaptiveReGroup nubAlphabets 16 
            mapChars = V.map V.length finalAlphs
            m = genMasks dummy blocklens mapChars alphlen numChars pMode parMode
        in PackedInfo {
                        blockLenMap = blocklens, 
                        bitAlphs = finalAlphs,
                        shuffleChars = shuffle,
                        specialMap = special,
                        maxAlphabet = overallAlph,
                        totalChars = numChars,
                        blockChars = mapChars,
                        masks = m} 
    | pMode == A64 = 
        let 
                (blocklens, finalAlphs, shuffle) = adaptiveReGroup nubAlphabets 64 
                mapChars = V.map V.length finalAlphs
                m = genMasks dummy blocklens mapChars alphlen numChars pMode parMode
            in PackedInfo {
                            blockLenMap = blocklens, 
                            bitAlphs = finalAlphs,
                            shuffleChars = shuffle,
                            specialMap = special,
                            maxAlphabet = overallAlph,
                            totalChars = numChars,
                            blockChars = mapChars,
                            masks = m} 
    | pMode == S16 || pMode == S64 = 
        let 
            m = genMasks dummy V.empty V.empty alphlen numChars pMode parMode
        in PackedInfo {
                        blockLenMap = V.empty, 
                        bitAlphs = V.empty,
                        shuffleChars = V.empty,
                        specialMap = special,
                        maxAlphabet = overallAlph,
                        totalChars = numChars,
                        blockChars = V.empty,
                        masks = m} 
    | otherwise = error "incorrect packing mode in get pack info"
        where
            numChars = length $ snd $ head pairedData
            recodeAlphabets = map (\alph -> foldr (\char acc -> if char `elem` (M.keys special) then acc ++ (special M.! char) else acc ++ [char]) [] alph) alphabets
            nubAlphabets = map nub recodeAlphabets
            overallAlph = special M.! '?'
            alphlen = length overallAlph

-- | Create the reduced alphabets based on what's seen at each character
-- this is an odd recursion where the head of each node is grabbed and it recurses on the tail of each node
genSeenAlphabets :: [TermData] -> Int -> SeenAlphs
--genSeenAlphabets terminals numChars charInfo | trace ("info " ++ show charInfo) False = undefined
genSeenAlphabets terminals numChars 
    | null $ snd $ head terminals = []
    | otherwise = 
        let
            heads = map (\(_, chars) -> head chars) terminals
            tails = map (\(n, chars) -> (n, tail chars)) terminals
            altogether = nub (map head heads)
            toSee = genSeenAlphabets tails numChars 
        in altogether : toSee

-- | Get the seen alphabets by a map over each index for parallelization
--genAlphMap :: [TermData] -> Int -> SeenAlphs
--genAlphMap terminals numChars = map (\i -> genAlphAtI terminals numChars i) [0..numChars] `using` (parListChunk 8 rseq)

---- | Create the reduced alphabets at an index
--genAlphAtI :: [TermData] -> Int -> Int -> [Char]
--genAlphAtI terminals numChars index 
--    | index >= numChars = []
--    | otherwise = nub $ map (\(_, chars) -> head $ chars !! index) terminals

-- | Function to correctly map the question mark to all characters
-- uses the charInfo as meaningful, but has a fall through in case
getSpecial :: SeenAlphs -> CharInfo -> M.Map Char String
getSpecial alphabets charInfo 
    | not (null $ alphabet charInfo) = --trace "use charInfo" $
        let 
            alph = map head (alphabet charInfo)
            finalAlph = [x | x<-alph, not $ x `elem` "?-"]
        in M.fromList [('?', finalAlph), ('-', finalAlph)]
    | otherwise = --trace "use alphs" $ 
        let 
            subAlphs = map (\alph -> [x | x<-alph, not $ x `elem` "?-"]) alphabets
            longestAlph = head $ sortBy (\a1 a2 -> if length a1 < length a2 then GT else LT) subAlphs
        in M.fromList [('?', longestAlph), ('-', longestAlph)]

-- | Do the regrouping process to put characters with the same length of seen alphabet together
-- also develops other relevant information and splits into the necessary number for packing
adaptiveReGroup :: [[Char]] -> Int -> (BlockLenMap, BitAlphabets, ShuffleChars)
adaptiveReGroup alphabets groupLen =
    let
        maxlen = foldr (\alph acc -> if (length alph) > acc then length alph else acc) 0 alphabets
        (alphGroups, groupMembers) = reGroupChars alphabets maxlen maxlen
        nbAlphs = [a | a<- alphGroups, a /= []]
        nbMembs = [m | m<-groupMembers, m /=[]]
        numChars = map length nbMembs
        (splitAlphGroups, splitGroupMembers) = --trace ("split with numChars " ++ show numChars ++ "and num groups " ++ show (length nbAlphs))
                                                splitAtInt (nbAlphs, nbMembs) groupLen numChars
        finalAlphs = V.fromList $ map V.fromList splitAlphGroups
        blocklens = V.fromList $ map (\g -> length $ head g) splitAlphGroups
        shuffle = V.fromList splitGroupMembers
    in (blocklens, finalAlphs, shuffle)

---- | regroup as a map instead of recursion for parallelization
--reGroupPar :: [String] -> Int -> ([[String]], [[Int]])
--reGroupPar inAlph maxlen = 
--    let 
--        iter = [1..maxlen]
--        raw = map (\i -> reGroupAtI inAlph i) iter `using` (parListChunk 4 rpar) :: [([String], [Int])]
--        indices = foldr (\(_, i) acc -> i : acc) [] raw
--        alphabets = foldr (\(alph, _) acc -> alph : acc) [] raw 
--    in (alphabets, indices)

--    where 
--        -- |Regroup at a single index
--        reGroupAtI :: [String] -> Int -> ([String], [Int])
--        reGroupAtI alphabets searchLen = 
--            let             
--                indices = [i | i <- [0..length alphabets - 1], (length $ alphabets !! i) == searchLen]
--                matchAlphabets = map (\i -> alphabets !! i) indices
--            in (matchAlphabets, indices)
        
-- | Lower-level function to regroup for a single seen alphabet length and recurse
reGroupChars :: [[Char]] -> Int -> Int -> ([[String]], [[Int]])
--reGroupChars alphabets string searchLen | trace ("re group chars with searchlen " ++ show searchLen ++ " and string length " ++ show (length string)) False = undefined
reGroupChars alphabets searchLen maxlen 
    | searchLen > maxlen = ([], [])
    | otherwise = 
        let 
            indices = [i | i <- [0..length alphabets - 1], (length $ alphabets !! i) == searchLen]
            matchAlphabets = --trace ("indices " ++ show indices ++ " and alphabets " ++ show alphabets)
                            map (\i -> alphabets !! i) indices
            (alphGroups, groupMembers) = reGroupChars alphabets (searchLen + 1) maxlen
            ret@(allAlphs, allMembs) = (matchAlphabets : alphGroups, indices : groupMembers)
        in ret

-- | Function to split groups at the word length for packing
-- determines how many fit based on their seen alphabets and recurses to split an entire grouping
splitAtInt :: ([[[Char]]], [[Int]]) -> Int -> [Int] -> ([[[Char]]], [[Int]])
--splitAtInt (inAlph, inGroups) sPoint numChars | trace ("split at int " ++ show inGroups ++ " and groups " ++ show inAlph ++ " and num chars " ++ show numChars) False = undefined
splitAtInt (inAlph, inGroups) sPoint numChars
    | null inAlph || null inGroups || null numChars = ([], [])
    | totalLength <= sPoint = --trace ("total length under 16") $
        let (restAlph, restGroups) = splitAtInt (tail inAlph, tail inGroups) sPoint (tail numChars)
        in (head inAlph : restAlph, head inGroups : restGroups)
    | otherwise =  --trace ("total length over 16") $
        let 
            leftovers = (drop takeNum (head inAlph) : tail inAlph, drop takeNum (head inGroups) : tail inGroups)
            newChars = (head numChars - takeNum) : (tail numChars)
            (restAlph, restGroups) = splitAtInt leftovers sPoint newChars
        in (take takeNum (head inAlph) : restAlph, take takeNum (head inGroups) : restGroups)

        where 
            nc = head numChars
            alphlen = length $ head $ head inAlph
            totalLength = nc * alphlen
            takeNum = --trace ("total length for split " ++ show totalLength) 
                        (div sPoint alphlen)


-- | Creates a leaf matrix for a packed forest given the raw data, simply a recursion of charSetToPacked
createPackedLeaves :: (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => a -> ParPackMode -> PackedInfo a -> RawData -> ParMode -> PackedTree a
--createPackedLeaves dummy pMode pInfo (pairedData, infoList) par | trace ("packed leaves " ++ show (length infoList)) False = undefined
createPackedLeaves dummy pMode pInfo (pairedData, infoList) par
    | null pairedData = V.empty
    | otherwise = 
        let indices = [0..length pairedData - 1]
        in (charSetToPacked dummy (snd $ head pairedData) (head infoList) pInfo pMode par) `V.cons` (createPackedLeaves dummy pMode pInfo ((tail pairedData), infoList) par)


-- | Converts a single node into a packed node
charSetToPacked :: (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => a -> [String] -> CharInfo -> PackedInfo a -> ParPackMode -> ParMode -> ParEnabledNode a
--charSetToPacked dummy strings charInfo pInfo pMode par | trace ("char set to packed " ++ show strings) False = undefined
charSetToPacked dummy strings charInfo pInfo pMode par
    | ((length strings) == 0) || ((head strings) == "no_data") = EmptyPackNode
    | charType charInfo == Add = error "packing only available for non-additive characters"
    | (length strings /= totalChars pInfo) = error "inconsistent number of characters, unsuitable for bit packing"
    | otherwise = makeNode dummy strToChar (specialMap pInfo) (bitAlphs pInfo) (maxAlphabet pInfo) (shuffleChars pInfo) pMode par
        where
            strToChar = map (\str -> head str) strings

-- | Uses a tree traversal to order the packed nodes into a packed tree, 
-- filling in empties elsewhere for a full structure with correct indices
packTree :: (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => [String] -> PackedTree a -> PhyloComponent -> PackedTree a
--packTree names leaves allNodes | trace ("packTree" ++ show names ++ show leaves) False = undefined 
packTree names leaves allNodes
    | (V.length allNodes == 0 ) = V.empty
    | not (myName `elem` names) = EmptyPackNode `V.cons` packTree names leaves (V.tail allNodes)
    | otherwise = 
        let 
            namePos = fromJust $ myName `elemIndex` names
            myNode = leaves V.! namePos
        in myNode `V.cons` packTree names leaves (V.tail allNodes)  
        where 
            myName = nodeName $ V.head allNodes


