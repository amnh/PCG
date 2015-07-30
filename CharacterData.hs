{-- |
Module      :  Functions for processing character data
Description :  Takes data from parser functins and recodess into usable state 
Copyright   :  (c) 2014 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :  

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}

module CharacterData
( createBaseData
, printDataMatrixVLS
, checkGraphAndData
, BaseChar
, CharacterSetList
, CharacterSet
, DataMatrixVLS
, redoRootCosts
, areCycles
) where

import System.IO
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Data.Int
import Data.Char
import Data.Bits
import qualified Data.Set as Set
import ReadFiles
import ReadGraphs
import Control.Parallel.Strategies
import Control.DeepSeq

--BasedData type array of list of vector of Int64
type BaseData = RawData

--types for character data, only BaseChar is "storable" so potentially interoperable with C
--characterSet better as list perhaps to allow for parallel map (data parallel) 
type BaseChar = (VS.Vector Int64)               --Vector so O(1) access, Int64 so can do bitwise stuff, Storable so can do FFI-C length 1-many
type CharacterSet = (V.Vector BaseChar)         --O(1) charcater acess--prob not need likely sequential so list OK
type CharacterSetList = [BaseChar]              --List so can easily parallel map functinos over data
type DataMatrix = (V.Vector CharacterSet)       --Vector so O(1) random access
type DataMatrixVLS = (V.Vector CharacterSetList) --BVLS = Vector-List-Storable

--convertToBit converts an In to bit re 0=1, 1=2, 2=4 etc
convertToBit :: Int -> Int64
convertToBit x 
    | x > 63 = error "State to high to convert to bit representation > 63"
    | x < 0  = error "State negative"
    | otherwise =
       shift 1 x

-- | convertDNASeqToBit takes DNA sequnce and returns Storable Vector of Int64 of bit
--representations (-=0, Aa = 1, Cc = 2, Gg=4 etc with IUPAC amibuities)
--currelty gap ambiguities not allow in this convertion other than ?
--Should make ACGT- but types and add for ambiguities
convertDNASeqToBit :: String -> BaseChar
convertDNASeqToBit x 
    | null x = VS.empty
    | toUpper (head x) == 'A' = VS.cons (1 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'C' = VS.cons (2 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'G' = VS.cons (4 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'T' = VS.cons (8 :: Int64) (convertDNASeqToBit (tail x))
--    | head x == '-' = VS.cons (16 :: Int64) (convertDNASeqToBit (tail x))
--    strips out gaps for now need to add prealigned data type option later
    | head x == '-' =  (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'R' = VS.cons (5 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'Y' = VS.cons (10 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'M' = VS.cons (3 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'K' = VS.cons (12 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'W' = VS.cons (9 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'S' = VS.cons (6 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'B' = VS.cons (14 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'D' = VS.cons (13 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'H' = VS.cons (11 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'V' = VS.cons (7 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'N' = VS.cons (15 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == 'X' = VS.cons (15 :: Int64) (convertDNASeqToBit (tail x))
    | toUpper (head x) == '?' = VS.cons (31 :: Int64) (convertDNASeqToBit (tail x))
    | otherwise = error ("Unreconized seqeunce character code " ++ show (head x)) 

-- | getListLengths sums lengths of characters in colummn of characters
-- since its a list awful n access, but only once, maybe change to Vector
getListLengths ::  DataMatrixVLS -> Int -> Int
getListLengths phyloData charNum =
    if V.null phyloData then 0
    else 
        let rowTerminal = V.head phyloData
        in
        (VS.length (rowTerminal !! charNum)) + (getListLengths (V.tail phyloData) charNum)


-- | redoRootCosts resets root costs for seqeunce characters--for now to 1/2
-- length by default
redoRootCosts :: DataMatrixVLS -> [CharInfo] -> Int -> [CharInfo] 
redoRootCosts phyloData charInfoList charNum =
    if null charInfoList then []
    else 
        let numTerminals = V.length phyloData
            sumLengthChar = getListLengths phyloData charNum 
            newRootCost = 0.5 * (fromIntegral sumLengthChar) / (fromIntegral numTerminals)
            newCharInfo = modifyRootCost (head charInfoList) newRootCost 
        in
        newCharInfo : redoRootCosts phyloData (tail charInfoList) (charNum + 1)

-- | convertGenSeqToBit takes custom alphabet sequnce and returns Storable Vector of Int64 of bit
--representations
--currelty gap ambiguities not allow in this convertion other than ?
--Should make 
--for now just in order of seeing them--later with matrix file
--set '-' to 64th bit
--throw error if > 63 states
--USE convertToBit
convertGenSeqToBit :: [String] -> [String] -> BaseChar
convertGenSeqToBit x alphabet =
    if null x then VS.empty
    else 
        let curState = head x
            bitNum = fromJust (elemIndex curState alphabet)
            bitChar = (bit bitNum) :: Int64  
        in
        VS.cons bitChar  (convertGenSeqToBit (tail x) alphabet)


-- | charSetToVestList converts, recursively, chars to vectors
charSetToVectList :: [String] -> [CharInfo] -> CharacterSetList 
charSetToVectList x charInfo 
    | null x    = []
    | otherwise =
        charSetToVect (head x) (head charInfo) : charSetToVectList (tail x) (tail charInfo)

-- | charToBaseChar takes list of input elements and list of char infomation and returns Vector of recoded
--base characters that should be ready for analysis
--"no_data" is missing data message so sets all bits to '0'
--need to recode ambiguities correctly--read states set each bit in char state
--(Int)
charSetToVect :: String -> CharInfo -> BaseChar 
charSetToVect x charInfo
    | null x = VS.empty
    | ((charType charInfo == NonAdd) || (charType charInfo == Add)) && ((head x == '?') || (head x == '-') || (x == "no_data")) =
        VS.singleton (maxBound :: Int64) --all '1' missing data
    | charType charInfo == Add = 
            VS.singleton (fromIntegral (digitToInt (head x)) :: Int64) --works through hex 0-9, a-z, A-Z
    | charType charInfo == NonAdd = 
        VS.singleton (convertToBit (digitToInt (head x)))
    | (charType charInfo == NucSeq) && (x == "no_data") = 
        VS.empty
    | (charType charInfo == NucSeq) = 
        convertDNASeqToBit x
    | (charType charInfo == GenSeq) && (x == "no_data") = 
        VS.empty
    | (charType charInfo == GenSeq) = 
        convertGenSeqToBit (words x) (alphabet charInfo) --Top x 
    | otherwise = error ("Char type " ++ show charInfo ++ " not implemented")

-- | termToVector takes a pairs of terminal and data (and dat info) and
--creates Vector of that pairData
--this is curried--reccomended by hlint
termToVector :: TermData -> [CharInfo] -> CharacterSetList
termToVector (name, dataList) = charSetToVectList dataList

-- | termToVectorList takes list of pairs of terminal and data (and dat info) and
--creates Vector of list of pairData recusively
termToVectorList :: [TermData] -> [CharInfo] -> [CharacterSetList]
termToVectorList x charInfo =
   if null x then []
   else 
         termToVector (head x) charInfo :  termToVectorList (tail x) charInfo

-- | createBaseData takes data from data file parser functions and creates
--a base data format of array of list of storable vector of Int64
--leaves (outdegree =0) are enumerated [0, (n-1)], then roots and other vertices
--Forests should hold their own non-leaf data, leaf data only one set to save
--space/copy time, but prob not big a deal.
--Creates a Vector for leaf names
createBaseData :: RawData -> DataMatrixVLS
createBaseData (pairedData, charInfo) = --testDataMatrixVLS
    if null pairedData then V.empty
    else V.fromList (termToVectorList pairedData charInfo)

-- | printSingelChar outputs singel string char
printSingleChar :: BaseChar -> IO ()
printSingleChar x =
    if VS.null x then hPutStr stderr " | "
    else
        do
            hPutStr stderr (show (VS.head x) ++ " ")
            printSingleChar (VS.tail x)

-- | printRowChars prints strings for given row
printRowChars :: CharacterSetList -> IO ()
printRowChars x =
    if null x then hPutStr stderr ""
    else
        do
            printSingleChar (head x)
            --hPutStr stderr (show $ head x)
            printRowChars (tail x)

--printRowsMatrixVLS prints taxon row
printRowsMatrixVLS :: DataMatrixVLS -> Int -> [String] -> IO ()
printRowsMatrixVLS x rowNum termList =
    if V.null x then hPutStr stderr ""
    else
        do
            hPutStr stderr (show rowNum ++ " " ++ head termList  ++ ": ")
            printRowChars (V.head x)
            hPutStr stderr "\n"
            printRowsMatrixVLS (V.tail x) (rowNum + 1) (tail termList)
 
--printDataNatrixVLS prints DataMatrix
printDataMatrixVLS :: DataMatrixVLS -> [String] -> IO ()
printDataMatrixVLS x termList =
    if V.null x then hPutStr stderr ""
    else
        do
            hPutStrLn stderr "DataMatrixVLS:" 
            printRowsMatrixVLS x 0 termList 

--termFromNode gets outdegree 0 nodes
termFromNode :: GenPhyNetNode -> String
termFromNode x =
    let (a, c, _) = x
    in
    if null c then a
    else "HopeThereIsNeverAtaxonWithThisNamer"

--termFromNet gets terminals from Network
termFromNet :: GenPhyNet -> [String]
termFromNet x =
    if null x then []
    else
        termFromNode (head x) : termFromNet (tail x)

--getTerminals extract outdegree 0 nodes
getTerminals :: GenForest -> [String]
getTerminals x =
    if null x then []
    else 
        termFromNet (head x) ++ getTerminals (tail x) 

-- | checkGraphAndData checks if termal leaf set of inputgraphs is same as
--taxon list
checkGraphAndData :: Set.Set String -> [GenForest] -> Bool
checkGraphAndData terminals graphList
    | Set.null terminals = error "No terminals in list"
    | null graphList = True
    | ((Set.difference terminals graphTermSet) /= Set.empty) || ((Set.difference graphTermSet terminals) /= Set.empty) =
        trace ("\n" ++ show  terminals ++ "\n" ++ show graphTermList ++ "\nSet diff: " 
        ++ show (Set.union (Set.difference terminals graphTermSet) (Set.difference graphTermSet terminals))) False
    | otherwise = checkGraphAndData terminals (tail graphList)
        where
            graphTermList = filter ('#' `notElem`) $ filter (/= "HopeThereIsNeverAtaxonWithThisNamer") $ getTerminals (head graphList)
            graphTermSet = Set.fromList graphTermList


-- | areCycles takes a list of GenForest and checks if there are cyles in each
-- component.  If there are cycles, errors with cycle info.a
-- this could be parallelized using parMap
-- null check should work because of laziness
areCycles :: [GenForest] -> Bool
areCycles inForestList
    | null inForestList = error "Empty forest list in areCycles"
    | any (==True) cyclesList = True
    | otherwise = False
        where cyclesList = map checkForCycles inForestList

-- | getFirstTwo takes a triple anc converts to pairs with first two
getFirstTwo :: (a, b, c) -> (a, b)
getFirstTwo (first, second, third) = (first, second)

-- | checkForCycles inputs a GenForest and checks components for cycles
-- this is stupidly O(n^3) could be O(n^2) I think by reusing desc lists 
checkForCycles :: GenForest -> Bool
checkForCycles inForest = False
    {- --| null inForest = error "Null input in checkForCyles" --trace ("\nInForest " ++ show allNodes ++ "\nDesc " ++ show nodeDescList) (
    --| head nodeDescList == ("", []) = True
    --| otherwise = False
    --    where
    --        allNodes = map getFirstTwo $ concat inForest --flatten nodes to one list
    --        nodeDescList = parMap rdeepseq (getDescendantList $ allNodes) allNodes
-}
---- | getDescendantList takes  node and tracks descdents adding all--not just
---- leaves
--getDescendantList :: [(String, [String])] -> (String, [String]) -> (String, [String])
--getDescendantList allNodeList inNode =
--    if null allNodeList then error "Null input in getDescendantList"
--    else
--        let (nodeName, descList) = inNode
--        in
--        if (intersect [nodeName] descList) /= [] then error ("Cycle found involving " ++ show (intersect [nodeName] descList)) 
--        else
--            let allDescList = descList ++ (onlyDescendantList allNodeList descList [nodeName])
--            in
--            (nodeName, allDescList)

-- | getDescendantList takes  node and tracks descdents adding all--not just
-- leaves
getDescendantList :: [(String, [String])] -> (String, [String]) -> (String, [String])
getDescendantList allNodeList inNode
    | null allNodeList                      = error "Null input in getDescendantList"
    | (intersect [nodeName] descList) /= [] = error ("Cycle found involving " ++ show (intersect [nodeName] descList)) 
    | otherwise                             = (nodeName, allDescList)
        where
            (nodeName, descList) = inNode
            allDescList = descList ++ (onlyDescendantList allNodeList descList [nodeName])

-- | onlyDescendantList takes a list of names, fileds the nodes among all nodes
-- and returns descdant list
onlyDescendantList :: [(String, [String])] -> [String] -> [String] -> [String]
onlyDescendantList allNodeList descNodeList rootNodeNameList
    | null descNodeList                                 = []
    | descNode == Nothing                               = error ("Node with name " ++ (head descNodeList) ++ " not found in onlyDescendantList")
    | (intersect rootNodeNameList newDescList) /= []    = error ("Cycle found involving  " ++ show (intersect rootNodeNameList newDescList))
    | otherwise = --trace ("\nNNList " ++ show newDescList)
        newDescList ++ (onlyDescendantList allNodeList newDescList $ rootNodeNameList ++ newDescList) 
        ++ (onlyDescendantList allNodeList (tail descNodeList) $ rootNodeNameList ++ newDescList)
        where
            descNode = lookup (head descNodeList) allNodeList 
            newDescList = fromJust descNode








