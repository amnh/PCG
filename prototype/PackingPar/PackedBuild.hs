
module PackedBuild (performPack, 
						PackedInfo (blockLenMap, bitAlphs, shuffleChars, specialMap, maxAlphabet, totalChars, blockChars, masks), 
						PackedTree, 
						PackedForest) where

import qualified BitPackedNode as BN
import qualified Data.Vector as V
import CharacterData
import ReadFiles
import Component
import Debug.Trace
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Control.Parallel.Strategies

-- | Define necessary types, mainly useful structures of nodes
type SeenAlphs = [[Char]]
type PackedTree = V.Vector BN.BitPackedNode
type PackedForest = V.Vector PackedTree
type Masks  = (BN.BitPackedNode, BN.BitPackedNode)
type BlockLenMap = V.Vector Int
type BitAlphabets = V.Vector (V.Vector [Char])
type ShuffleChars = V.Vector [Int]
type SpecialMap = M.Map Char String


-- | Data types for info on the pack.  Note that for static packing not all PackedInfo fields will be useful or filled
data PackedInfo = PackedInfo 	{ blockLenMap :: BlockLenMap
								, bitAlphs :: BitAlphabets
								, shuffleChars :: ShuffleChars
								, specialMap :: SpecialMap
								, maxAlphabet :: [Char]
								, totalChars :: Int
								, blockChars :: V.Vector Int
								, masks :: Masks
								} deriving (Show, Eq)


-- | Standard special characters for genetic and amino acid sequences, based on standard codes
specialCharsGene = M.fromList [('?', "ACGT-"), ('R', "AG"), ('Y', "CT"), ('S', "GC"), ('W', "AT"), ('K', "GT"), ('M', "AC"), ('B', "CGT"), 
					('D', "AGT"), ('H', "ACT"), ('H', "ACT"), ('V', "ACG"), ('N', "ACGT")]
specialCharsNuc = M.fromList [('?', "ACDEFGHIKLMNPQRSTVWY")]

-- | Master function to determine the mode and do all of the packing after checking for incorrect data types
performPack :: RawData -> [String] -> PhyloForest -> (String, String) -> (PackedForest, PackedInfo, BN.PackMode)
performPack raw@(pairedData, infoList) names forest inMode 
	| charType (head infoList) == Add || charType (head infoList) == Matrix = error "bit packed optimization only handles non-additive characters"
	| otherwise = 
		let 
			numChars = length $ snd $ head pairedData
			alphabets = genAlphMap pairedData numChars
			special = --trace ("alphabets " ++ show (length alphabets))
						getSpecial alphabets (head infoList)
			overallAlph = special M.! '?'
			pMode = getPackMode inMode overallAlph
			pInfo = getPackInfo raw pMode alphabets special
			leaves = createPackedLeaves pMode pInfo raw names
			packForest = V.fromList $ map (\tree -> packTree names leaves tree) forest
		in --trace ("packed forest " ++ show packForest)
				(packForest, pInfo, pMode)


-- | Helper function to take string input and output the BN.PackMode object, automatically determining as needed
getPackMode :: (String, String) -> [Char] -> BN.PackMode
getPackMode (genMode, bitLen) alphabet
	| genMode == "automatic" = setAutoPack alphlen
	| bitLen == "16" && alphlen > 16 = trace ("bit length set to 16 but alphabet length is " ++ show alphlen ++ " auto packing instead")
										setAutoPack alphlen
	| bitLen == "64" && alphlen > 64 = trace ("bit length set to 64 but alphabet length is " ++ show alphlen ++ " auto packing instead")
										setAutoPack alphlen
	| genMode == "adaptive" && bitLen == "16" = BN.MakePackMode {BN.bitLen = 16, BN.adaptive = True}
	| genMode == "adaptive" && bitLen == "64" = BN.MakePackMode {BN.bitLen = 64, BN.adaptive = True}
	| genMode == "static" && bitLen == "16" = BN.MakePackMode {BN.bitLen = 16, BN.adaptive = False}
	| genMode == "static" && bitLen == "64" = BN.MakePackMode {BN.bitLen = 64, BN.adaptive = False}
	| genMode == "static" && bitLen == "infinite" = BN.MakePackMode {BN.bitLen = 0, BN.adaptive = False}
	| otherwise = trace ("pack mode set incorrectly, performing auto mode pack") (setAutoPack alphlen) 
		where alphlen = length alphabet

-- | Helper function to determine the best packing mode for some data
-- based on early results it looks like static 64 performs best, but further use cases could change this
setAutoPack :: Int -> BN.PackMode
setAutoPack alphlen
	| alphlen <= 64 = BN.MakePackMode {BN.bitLen = 64, BN.adaptive = False}
	| otherwise = BN.MakePackMode {BN.bitLen = 0, BN.adaptive = False}

-- | Creates the pack info structure, filling in everything for the word types (in which case it won't be evaluated because of laziness)
-- and only giving the infinite what's meaningful
getPackInfo :: RawData -> BN.PackMode -> SeenAlphs -> SpecialMap -> PackedInfo
getPackInfo raw@(pairedData, infoList) pMode alphabets special 
	| BN.bitLen pMode == 16 = 
		let 
			regroup@(blocklens, finalAlphs, shuffle) = adaptiveReGroup nubAlphabets special 16 numChars
			mapChars = V.map V.length finalAlphs
			m@(occMask, sMask) = BN.genMasks blocklens mapChars alphlen numChars pMode
		in PackedInfo {
						blockLenMap = blocklens, 
						bitAlphs = finalAlphs,
						shuffleChars = shuffle,
						specialMap = special,
						maxAlphabet = overallAlph,
						totalChars = numChars,
						blockChars = mapChars,
						masks = m} 
	| BN.bitLen pMode == 64 = 
		let 
				regroup@(blocklens, finalAlphs, shuffle) = adaptiveReGroup nubAlphabets special 64 numChars
				mapChars = V.map V.length finalAlphs
				m@(occMask, sMask) = BN.genMasks blocklens mapChars alphlen numChars pMode
			in PackedInfo {
							blockLenMap = blocklens, 
							bitAlphs = finalAlphs,
							shuffleChars = shuffle,
							specialMap = special,
							maxAlphabet = overallAlph,
							totalChars = numChars,
							blockChars = mapChars,
							masks = m} 
	| BN.bitLen pMode == 0 = 
		let 
			m@(occMask, sMask) = BN.genMasks V.empty V.empty alphlen numChars pMode
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
			numChars = --trace ("numchars head " ++ show pairedData)
						length $ snd $ head pairedData
			recodeAlphabets = map (\alph -> foldr (\char acc -> if char `elem` (M.keys special) then acc ++ (special M.! char) else acc ++ [char]) [] alph) alphabets
			nubAlphabets = map nub recodeAlphabets
			overallAlph = --trace ("alphabets " ++ show nubAlphabets)
							special M.! '?'
			alphlen = length overallAlph

-- | Create the reduced alphabets based on what's seen at each character
-- this is an odd recursion where the head of each node is grabbed and it recurses on the tail of each node
genSeenAlphabets :: [TermData] -> Int -> SeenAlphs
--genSeenAlphabets terminals numChars charInfo | trace ("info " ++ show charInfo) False = undefined
genSeenAlphabets terminals numChars 
	| null $ snd $ head terminals = []
	| otherwise = 
		let
			heads = map (\(name, chars) -> head chars) terminals
			tails = map (\(name, chars) -> (name, tail chars)) terminals
			allTogether = nub (map head heads)
			toSee = genSeenAlphabets tails numChars 
		in allTogether : toSee

-- | Get the seen alphabets by a map over each index for parallelization
genAlphMap :: [TermData] -> Int -> SeenAlphs
genAlphMap terminals numChars = map (\i -> genAlphAtI terminals numChars i) [0..numChars] `using` (parListChunk 8 rseq)

-- | Create the reduced alphabets at an index
genAlphAtI :: [TermData] -> Int -> Int -> [Char]
genAlphAtI terminals numChars index 
	| index >= numChars = []
	| otherwise = nub $ map (\(name, chars) -> head $ chars !! index) terminals

-- | Function to correctly map the question mark to all characters
-- uses the charInfo as meaningful, but has a fall through in case
getSpecial :: SeenAlphs -> CharInfo -> M.Map Char String
getSpecial alphabets charInfo 
	| charType charInfo == GenSeq = specialCharsGene
	| charType charInfo == NucSeq = specialCharsNuc
	| not (null $ alphabet charInfo) = trace "use charInfo" $
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
adaptiveReGroup :: [[Char]] -> M.Map Char String -> Int -> Int -> (BlockLenMap, BitAlphabets, ShuffleChars)
adaptiveReGroup alphabets special groupLen numChars =
	let
		maxlen = foldr (\alph acc -> if (length alph) > acc then length alph else acc) 0 alphabets
		groups@(alphGroups, groupMembers) = reGroupPar alphabets maxlen
		nbAlphs = [a | a<- alphGroups, a /= []]
		nbMembs = [m | m<-groupMembers, m /=[]]
		numChars = map length nbMembs
		(splitAlphGroups, splitGroupMembers) = --trace ("split with numChars " ++ show numChars ++ "and num groups " ++ show (length nbAlphs))
												splitAtInt (nbAlphs, nbMembs) groupLen numChars
		finalAlphs = V.fromList $ map V.fromList splitAlphGroups
		blocklens = V.fromList $ map (\g -> length $ head g) splitAlphGroups
		shuffle = V.fromList splitGroupMembers
	in (blocklens, finalAlphs, shuffle)

-- | regroup as a map instead of recursion for parallelization
reGroupPar :: [String] -> Int -> ([[String]], [[Int]])
reGroupPar inAlph maxlen = 
	let 
		iter = [1..maxlen]
		raw = map (\i -> reGroupAtI inAlph i) iter `using` (parListChunk 4 rpar) :: [([String], [Int])]
		indices = foldr (\(alph, i) acc -> i : acc) [] raw
		alphabets = foldr (\(alph, i) acc -> alph : acc) [] raw 
	in (alphabets, indices)

	where 
		-- |Regroup at a single index
		reGroupAtI :: [String] -> Int -> ([String], [Int])
		reGroupAtI alphabets searchLen = 
			let 			
				indices = [i | i <- [0..length alphabets - 1], (length $ alphabets !! i) == searchLen]
				matchAlphabets = map (\i -> alphabets !! i) indices
			in (matchAlphabets, indices)
		
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
createPackedLeaves :: BN.PackMode -> PackedInfo -> RawData -> [String] -> PackedTree 
createPackedLeaves pMode pInfo raw@(pairedData, infoList) names
	| (length pairedData == 0) = V.empty
	| otherwise = 
		let indices = [0..length pairedData - 1]
		in V.fromList $ parMap rpar (\i -> charSetToPacked (snd $ pairedData !! i) (infoList !! i) pInfo pMode (names !! i)) indices
		--(charSetToPacked (snd $ head pairedData) (head infoList) pInfo pMode (head names)) `V.cons` (createPackedLeaves pMode pInfo ((tail pairedData), infoList) (tail names))


-- | Converts a single node into a packed node
charSetToPacked :: [String] -> CharInfo -> PackedInfo -> BN.PackMode -> String -> BN.BitPackedNode
charSetToPacked strings charInfo pInfo pMode myName
	| ((length strings) == 0) || ((head strings) == "no_data") = BN.EmptyPackNode
	| charType charInfo == Add = error "packing only available for non-additive characters"
	| (length strings /= totalChars pInfo) = error "inconsistent number of characters, unsuitable for bit packing"
	| otherwise = BN.makeNode strToChar (specialMap pInfo) (bitAlphs pInfo) (maxAlphabet pInfo) (shuffleChars pInfo) pMode
		where
			strToChar = map (\str -> head str) strings

-- | Uses a tree traversal to order the packed nodes into a packed tree, 
-- filling in empties elsewhere for a full structure with correct indices
packTree :: [String] -> PackedTree -> PhyloComponent -> PackedTree
packTree names leaves allNodes
	| (V.length allNodes == 0 ) = V.empty
	| not (myName `elem` names) = BN.EmptyPackNode `V.cons` packTree names leaves (V.tail allNodes)
	| otherwise = 
		let 
			namePos = fromJust $ myName `elemIndex` names
			myNode = leaves V.! namePos
		in myNode `V.cons` packTree names leaves (V.tail allNodes)  
		where 
			myName = nodeName $ V.head allNodes


