{- |
Module      :  Command execution functions
Description :  Functions to process command arguments from file--or perhaps interactive input 
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

--should be compiled with -fno-cse and potentially -fno-full-laziness due to
--unsafePerformIO call

module ExecuteCommands
( executeCommandList
, executeCommand
, getReadContents
, getGraphContents
, flattenCharList
, getNameList
) where

import System.IO
import System.IO.Unsafe
import System.Process
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import ProcessCommands
import ReadFiles
import ReadGraphs
--import PhylogeneticGraphs
import qualified Data.Vector as V

--maxIntLocal = 1000000000

-- | executeCommand takes command tuple and executes
executeCommand :: Command -> IO ()
executeCommand x =
    if  fst x == "read" then do
        let readFileName = head (snd x)
        hPutStrLn stderr "Reading :"
        putStrLn readFileName
        --readFileHandle <- openFile readFileName ReadMode 
        --fileContents <- hGetContents readFileHandle
        fileContents <- readFile readFileName
        hPutStrLn stderr "Successfull"
    else 
        error ("Command " ++ show x  ++ " not recognizes/implememnted.")

-- | sendMessage is wrapper for update infor via IO ()
sendMessage :: String -> IO ()
sendMessage x =
        if null x then hPutStrLn stderr "Unspecified message"
        else putStrLn x

-- | executeCommandList executes each command in list returning inputData,
--processData, Forest tuple
executeCommandList :: [Command] -> IO ()    --(RawData, RawData, GenForest) -> (RawData, RawData, GenForest)
executeCommandList commandList =             --(inputData, processedData, inputForest) =
    if null commandList then hPutStrLn stderr "Done"
    else do
        hPutStrLn stderr ("Processing command" ++ show (head commandList))
        executeCommand (head commandList)
        executeCommandList (tail commandList)
    --(inputData, processedData, inputForest)

-- | removeBranchLengths takes Newick/ENeweick and strips out branch lengths
removeBranchLengths :: String -> String
removeBranchLengths inString =
    if null inString then error "Null input string"
    else 
        let firstSplit = splitOn "," inString
            secondSplit =  splitParen firstSplit
        in reassemble "," secondSplit

-- | reassemble takes [String] and adds arg between elements and returns String
reassemble :: String -> [String] -> String
reassemble joinString inList =
    if null joinString  then error "Join string is empty"
    else 
         if not (null $ tail inList) then (head inList) ++ (joinString ++ (reassemble joinString $ tail inList))
         else head inList

-- | splitParen takes list of String and splits each one on ')' and reassmebles
splitParen :: [String] -> [String]
splitParen inList = 
    if null inList then []
    else map (\x -> reassemble ")" (splitColon $ splitOn ")" x)) inList
        --let firstSplit = splitOn ")" (head inList)
        --    secondSplit  = splitColon firstSplit
        --    outList = reassemble ")" secondSplit
        --in
        --    outList : (splitParen $ tail inList)

-- | splitColon takes list of String and splits each one on ':'deletes branch
-- length after it 
splitColon ::[String] -> [String]
splitColon inList =
    if null inList then []
    else map (\x -> head $ splitOn ":" x) inList
        --let firstSplit = splitOn ":" (head inList)
        --    outList = head firstSplit
        --in
        --    outList : (splitColon $ tail inList)

{-# NOINLINE getGraphContents #-}
--getGraphContents inputs read commands, processes into data, and returns list of rawData
--uses unsafePerformIO--yuck need to remove
getGraphContents :: CommandList -> [GenForest]
getGraphContents cL 
    | null cL = []
    | code == "ver" = 
       let fileContents = readFile (head b) -- <- readFile (head b)
           y = unsafePerformIO fileContents
        in processVertexEdgeRoot y : getGraphContents (tail cL)
     | code == "newick" =  --really reads extended Newick (#Hi stuff)
       let fileContents = readFile (init $ head b) -- <- readFile (head b)
           y = unsafePerformIO fileContents
           z = processNewick (removeBranchLengths y)
           t = mergeNetNodes z
        in [t] : getGraphContents (tail cL)
    | code == "fen" = 
       let fileContents = readFile (init $ head b) -- <- readFile (head b)
           y = unsafePerformIO fileContents
        in processFEN (removeBranchLengths y) : getGraphContents (tail cL)
    | otherwise =  error "File format not recognized"
       where 
            (a, b) = head cL
            code = trace ("Reading graph file from " ++ show (head b)) (last b)


--mergeNetNodes merges nodes with same name and '#Hi" format (extended Newick)
mergeNetNodes :: GenPhyNet -> GenPhyNet 
mergeNetNodes inNodes =
    if null inNodes then []
    else 
        let allNames = getNodeNames inNodes --including repeats
            nodeNames = nub $ allNames 
            nameTimes = getNumNames nodeNames allNames
            newNodes = mergeAndPurge nameTimes (V.fromList inNodes) --so constant time random access
        in
        newNodes
        --error ("Name repeats " ++ show nodeNames ++ " " ++ show nameTimes)

--getMergeNames extract names from nodes
getMergeNames :: [Int] -> V.Vector GenPhyNetNode -> [String]
getMergeNames nameIndices inNodes =
    if null nameIndices then []
    else map (\x -> tfst (inNodes V.! x)) nameIndices
        where 
            tfst :: (a, b, c) -> a
            tfst (x, _, _) = x
        --let (a, _, _) = inNodes V.! (head) nameIndices
        --in
        --a : getMergeNames (tail nameIndices) inNodes

--getMergeDesc extract descList from nodes
getMergeDesc :: [Int] -> V.Vector GenPhyNetNode -> [String]
getMergeDesc nameIndices inNodes =
    if null nameIndices then []
    else 
        let (_, a, _) = inNodes V.! (head) nameIndices
        in a ++ getMergeDesc (tail nameIndices) inNodes

--getMergeAnc extract ancList from nodes
getMergeAnc :: [Int] -> V.Vector GenPhyNetNode -> [String]
getMergeAnc nameIndices inNodes =
    if null nameIndices then []
    else 
        let (_, _, a) = inNodes V.! (head) nameIndices
        in a ++ getMergeAnc (tail nameIndices) inNodes

--mergeAndPurge takes list of name occurences and 
--  if 1 time -> keep
--  if 2 times -> merge
--      purge the old nodes
mergeAndPurge :: [[Int]] -> V.Vector GenPhyNetNode -> GenPhyNet
mergeAndPurge nameTimes inNodes 
    | null nameTimes = []
    | occurences == 1 = (inNodes V.! (head curNameList)) : mergeAndPurge (tail nameTimes) inNodes
    | (occurences > 1) && (length name > 1) = error ("Problem in name list in mergeAndPurge" ++ show name)
    | occurences > 1 = (head name, descList, ancList) : (mergeAndPurge (tail nameTimes) inNodes)
    | otherwise = error "Problem in name list in mergeAndPurge"
        where 
            curNameList = head nameTimes 
            occurences = length curNameList

            name = nub $ getMergeNames curNameList inNodes
            descList = getMergeDesc curNameList inNodes
            ancList = getMergeAnc curNameList inNodes

--getNumNames takes list of names and nodes and returns the number of times eah
--name is found in the node list
getNumNames :: [String] -> [String] -> [[Int]]
getNumNames nameList allNames =
    if null nameList then []
    else 
        let name = head nameList
            found = findIndices ( == name)  allNames 
        in
        found : getNumNames (tail nameList) allNames

--getNodeNames extract name list from PhyloComponent
getNodeNames :: GenPhyNet -> [String]
getNodeNames inNodes =
    if null inNodes then []
    else 
        let (name, _ , _) = head inNodes
        in
        name : getNodeNames (tail inNodes)

--getDividingComma takes a newick string with no outer paren pair and
--identifies the position of the comma that divides the descdendents for now
--assumes binary
--basically adds left paren and subtracts right paren when encountered,
--if that number == 0 when a comma is encountered, that's the dividing comma 
getDividingComma :: String -> Int -> Int -> Int -> Int --should be list for non-binary
getDividingComma newickString numLeftParens numRightParens curPosition 
    | null newickString = (-1) --this for hitting a terminal name error "Error in binary Newick file--couldn't find comma break"
    | firstChar == '(' = getDividingComma (tail newickString) (numLeftParens + 1) numRightParens (curPosition + 1)
    | firstChar == ')' = getDividingComma (tail newickString) numLeftParens (numRightParens + 1) (curPosition + 1)
    | (firstChar == ',') && (numLeftParens == numRightParens) = curPosition
    | firstChar == ',' = getDividingComma (tail newickString) numLeftParens numRightParens (curPosition + 1)
    | otherwise = getDividingComma (tail newickString) numLeftParens numRightParens (curPosition + 1)
        where firstChar = head newickString

--stripLabel removes any non-label from taxon or subtree name
stripLabel :: String -> String
stripLabel inName =
    if null inName then error "No input string in stripLabel"
    else 
        let outName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse inName
        in
        --trace ("New desc " ++ show inName ++ " " ++ show outName) (
        if outName == "" then inName
        else outName
        --)

--stripBranchLengths take off branch lengths from lables -- part after ':'
stripBranchLengths :: String -> String
stripBranchLengths inLabel =
    if null inLabel then []
    else takeWhile (/= ':') inLabel


--getRestNewick takes from the root node set in processnewick and recurses
--through the subtrees until only terminals are found (no commas)
getRestNewick :: String -> String -> GenPhyNet
getRestNewick inSubTree ancTree 
    --trace ("\nGRN inSubTree: " ++ inSubTree ++ " \nanc " ++ ancTree) (
    | null inSubTree = error ("Error in Newick file parsing.  Anc: " ++ ancTree)
    | (notElem ',' inSubTree) && (head inSubTree /= '(') =  --trace ("\nTerminal " ++ show ancTree ++ "->" ++ show inSubTree) (
        [(inSubTree, [], [ancTree])] --commaPosition == maxIntLocal then
    | (notElem ',' inSubTree) && (null nodeName)= --is indegree=outdegree=1 node 
        --trace ("\n11 " ++ show inSubTree ++ " " ++ show nodeName ++ " " ++ show descNode ++ " " ++ show descNodeName) (
        [(inSubTree, [descNodeName], [ancTree])] ++ (getRestNewick descNodeName inSubTree) --)
    | (notElem ',' inSubTree) = [(nodeName, [descNodeName], [ancTree])] ++ (getRestNewick descNodeName nodeName)
    | null nodeName = --set node and recurse
        {-trace ("\n label stuff " ++ show nodeName ++ " " ++ show subTreeStripped ++ " " ++ show subTreeNoLabel 
             ++ " " ++ show commaPositionHere ++ "\nInternal " ++ show ancTree ++ "->" ++ show leftDesc ++ " and " 
            ++ show rightDesc ++ " label " ++ show nodeName ++ show (getDividingComma rightDesc 0 0 0)) (
        -}
        --trace ("posLR " ++ show commaPositionHere ++ " reverse " ++ show ((length subTreeNoLabel) - (getDividingComma (reverse subTreeNoLabel) 0 0 0) - 1) ++ show rightDesc) (
        [(inSubTree, [leftDescStripped, rightDescStripped], [ancTree])] ++ (getRestNewick leftDesc inSubTree ) 
            ++ (getRestNewick rightDesc inSubTree)
    | otherwise = [(nodeName, [leftDescStripped, rightDescStripped], [ancTree])] ++ (getRestNewick leftDesc nodeName) 
                    ++ (getRestNewick rightDesc nodeName)

        where 
            nodeName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse inSubTree
            descNode = reverse $ dropWhile (/= ')') $ reverse inSubTree
            descNodeName = stripLabel $ tail $ init descNode --getSisters descNode  --tail $ init $ descNode 
            
            subTreeStripped =  reverse $ dropWhile (/= ')') $ reverse inSubTree
            subTreeNoLabel = tail $ init $ subTreeStripped --getSisters subTreeStripped --tail $ init $ subTreeStripped
            commaPositionHere = getDividingComma subTreeNoLabel 0 0 0
            (leftDesc, preRightDesc) = splitAt commaPositionHere subTreeNoLabel --assumes binary
            rightDesc = dichotomize (tail preRightDesc) --removes ',' and resolves polytomies
            leftDescStripped = stripLabel leftDesc
            rightDescStripped = stripLabel rightDesc 


-- | dichotomize takes part of newick string and if more than one component adds
-- parens on outside.  THis effectively dichotomizes multi-tomies in newick
-- description
dichotomize :: String -> String
dichotomize inString =
    if null inString then error "Null in dichotomize"
    else 
        if (getDividingComma inString 0 0 0) /= (-1) then "(" ++ inString ++ ")"
        else inString

{-
 - -- | getSisters takes a paren defined string   '(' blah, blah2 ')' and strips
-- out leading and trailing parens.  Also checks if extraneous parens as in
-- created by Dendroscope
-- also issue of polytomies
getSisters :: String -> String
getSisters inString =
    if null inString then error ("Error in newick parsing 'getSisters'")
    else if (notElem ',' inString) then inString --leaf name
    --else if (getDividingComma inString 0 0 0) < maxIntLocal then trace ("Comma Inpos " ++ show (getDividingComma inString 0 0 0) ++ show inString) inString
    else tail $ init inString
       {- let candidate = tail $ init inString
            commaPosition = getDividingComma candidate 0 0 0
            (frontPart, backPart) = trace ("Comma pos " ++ show commaPosition ++ show candidate) splitAt commaPosition candidate
            numLeftFront = length $ elemIndices '(' frontPart
            numLeftBack = length $ elemIndices '(' backPart
            numRightFront = length $ elemIndices ')' frontPart
            numRightBack = length $ elemIndices '(' backPart
        in
        if (numLeftFront == numRightFront) && (numLeftBack == numRightBack) then candidate
        else  trace ("Extra parens in E/Newick: " ++ show inString ++ "  proceeding") getSisters candidate
    -}
-}

--processNewick reads Enhanced Newick files and returns GenForest
--for now only reads one tree (later splitOn ';' and return [GenForest])
processNewick :: String -> GenPhyNet
processNewick inNewickString =
                {-trace ("\nRoot label = " ++ show nodeName ++ "\n processed newick " ++ show reducedString ++ "->" 
                ++ show leftDesc ++ " " ++ show (tail rightDesc)) (
            -}
            --trace ("posLR " ++ show (getDividingComma rightDesc 0 0 0) ++ show rightDesc) (
    --check pren numbers match
    let newickString1 = filter (/= '\n') inNewickString --remove newlines
        newickString2 = filter (/= ' ') newickString1 --remove spaces
        newickString3 = filter (/= '\t') newickString2
        newickString = filter (/= '\r') newickString3
        leftParens = length $ elemIndices '(' newickString
        rightParens = length $ elemIndices ')' newickString
    
        y   | leftParens /= rightParens = error ("Parentheses do not match in Newick input" ++ show newickString)
            | not ((head newickString == '(') && (last newickString == ';')) = error "Newick tree must start with '(' and end with ';'"
            | (commaPosition /= (-1)) = 
                let 
                    x   | null nodeName = 
                            (gutsOnly, [leftDescStripped, rightDescStripped], []) :  ((getRestNewick leftDesc gutsOnly ) ++ (getRestNewick rightDesc gutsOnly)) 
                        | otherwise = 
                            (nodeName, [leftDescStripped, rightDescStripped], []) :  ((getRestNewick leftDesc nodeName ) ++ (getRestNewick rightDesc nodeName))
                            where
                                (leftDesc, preRightDesc) = splitAt commaPosition reducedString --assumes binary
                                rightDesc = tail preRightDesc --removes ','
                                leftDescStripped = stripLabel leftDesc
                                rightDescStripped = stripLabel rightDesc --check for polytomies here
                in x
            | null nodeName = (gutsOnly, [stripBranchLengths reducedString], []) : (getRestNewick reducedString gutsOnly)
            | otherwise = (nodeName, [stripBranchLengths reducedString], []) : (getRestNewick reducedString nodeName)
                where
                    gutsOnlyLabel = takeWhile (/= '[') $ init newickString --disgards comments after here, assumes only comment at end before ';'
                    nodeName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse gutsOnlyLabel
                    gutsOnly = reverse $ dropWhile (/= ')') $ reverse gutsOnlyLabel
                    nodeAnc = []
                    reducedString = tail $ init gutsOnly --getSisters gutsOnly --tail $ init gutsOnly
                    commaPosition = getDividingComma reducedString 0 0 0
    in y

--processNewickComponents takes list of parse newicks from fen file,
--add trailing ';' and processes newicks, returnin a forest
processNewickComponents :: [String] -> GenForest
processNewickComponents inNewickList =
    if null inNewickList then []
    else map (\x -> mergeNetNodes $ processNewick $ x ++ ";") inNewickList --; is to to make valid newick after ending ";"
        --let componentString = (head inNewickList) ++ ";" --to make valis newick after endby ";"
        --    newickNodes = processNewick componentString
        --    enhancedNewickNodes = mergeNetNodes newickNodes
        --in
        ----trace ("\nNewick Components " ++ show newickNodes) 
        --enhancedNewickNodes : (processNewickComponents (tail inNewickList))

--processFEN reads Forest Enhanced Newick files and returns GenForest
processFEN :: String -> GenForest
processFEN inForestString =
    if null inForestString then error "Empty fen file"
    else
        let forestString =  filter (/= ' ') $ filter (/= '\n') inForestString
            leftBrackets = length $ elemIndices '<' forestString
            rightBrackets = length $ elemIndices '>' forestString
        in
        if (head forestString == '<') && (last forestString == '*')  && (leftBrackets == 1) && (rightBrackets == 1) then 
           let  guts = tail $ takeWhile (/= '>') forestString
                components = endBy ";" guts
           in
           processNewickComponents components
           --error (show guts ++ " " ++ show components)
        else error "Not valid FEN format"

{-# NOINLINE getReadContents #-}
--getReadContents inputs read commands, processes into data, and returns list of rawData
--uses unsafePerformIO--yuck need to remove
getReadContents :: CommandList -> [RawData] --[IO String]
getReadContents cL 
    | null cL = []
    --check for file type here
    |  (length b == 1) = 
        let fileContents = readFile (head b) -- <- readFile (head b)
            y = unsafePerformIO fileContents
            x   | head y == '>' = processFastaInput y :  getReadContents (tail cL) --(processFileList (tail x)))
                | (head y == 'X') || (head y == 'x') = processTNTInput y :  getReadContents (tail cL) --(processFileList (tail x))) 
                | otherwise = error "Unrecognized input file format."
        in x
    | (length b == 2) = --there is a file type argument
        let 
            filetype = last b --assumes only two arguments
            z   | filetype == "custom_alphabet" = trace ("\ntcmFile " ++ show secondPart)
                    processCustomAlphabet fileStuff tcmFileStuff :  getReadContents (tail cL) 
                | otherwise = error "Unrecognized/implemented input filetype"
                    where 
                        fileName = takeWhile (/= '\"') (head b)
                        fileContents = readFile fileName
                        fileStuff = unsafePerformIO fileContents
                        secondPart = takeWhile (/= '\"') $ tail $ dropWhile (/= '\"') $ dropWhile (/= ',') (head b) 
                        tcmFileStuff = unsafePerformIO $ readFile secondPart
        in z
    | (length b > 2) = error "too many file arguments in read command"
    | otherwise = error "Error in command line parse."
        where (a, b) = head cL


--getTermDataList extracts List of List of TermData for 
--compilation
getTermDataList :: [RawData] -> [[TermData]]
getTermDataList x = 
    if null x then []
    else map (\a -> fst a) x
        --let termDataList = fst $ head x
        --in
        --termDataList : getTermDataList (tail x)

--getNameData for given name get data Strings
--if not found then add "no_data" missing data String
getNameData :: String -> [[TermData]] -> [String]
getNameData curName termDataListList =
    if null termDataListList then []
    else
        let retrievedStuff = lookup curName (head termDataListList)
        in
        if isNothing retrievedStuff then "no_data" : getNameData curName (tail termDataListList)
        else fromJust retrievedStuff ++ 
                getNameData curName (tail termDataListList)

--extractDataByName does through list of term info and extracts data ([String])
--and cons to existing--by name
extractDataByName :: [String] -> [[TermData]] -> [TermData]
extractDataByName searchNameList termDataListList =
    if null searchNameList then []
    else map (\x -> (x, getNameData x termDataListList)) searchNameList
        --let searchName = head searchNameList
        --    nameData =  getNameData searchName termDataListList 
        --    allTermData = (searchName, nameData) : extractDataByName (tail searchNameList) termDataListList
        --in
        --allTermData

--getCharInfo extracts and concats character Inof list from RawData List
getCharInfo :: [RawData] -> [CharInfo]
getCharInfo x =
    if null x then []
    else foldr (\b acc -> (snd b) ++ acc) [] x
        --let (baseTermData, baseCharInfo) = head x
        --    newCharInfo = baseCharInfo ++ getCharInfo (tail x)
        --in
        ----trace ("elided " ++ show (length newCharInfo) ++ " charInfo's")
        --newCharInfo

--getNameVect extracts terminals names from RawData
getNameList :: [TermData] -> [String]
getNameList x = 
    if null x then []
    else map fst x
        --let (newName , _) = head x
        --    nameList = newName : getNameList (tail x)
        --in
        ----trace ("Name list has " ++ show (length nameList) ++ " entries")
        --nameList 

--getNameListAll gets total name list over all inputs--helps with missing data
--etc in multiple input files
getNameListAll :: [[TermData]] -> [String]
getNameListAll x =
    if null x then []
    else foldr (\b acc -> (getNameList b) ++ acc) [] x
        --getNameList (head x) ++ getNameListAll (tail x)

--flattenCharList takes list of chartcatert info and flattens based on taxon
--name adding missing data as required
--for now assume same leaf set--need to imporve to allow for missing data etc
flattenCharList :: [RawData] -> RawData
flattenCharList x =
    if null x then error "Empty RawData list"
    else 
        let (baseTermData, baseCharInfo) = head x
            newCharInfo = baseCharInfo ++ getCharInfo (tail x)
            termDataListList = getTermDataList x
            termNameList = nub $ getNameListAll termDataListList
            newTermData = extractDataByName termNameList termDataListList
            flatData = (newTermData, newCharInfo)
        in
        (newTermData, newCharInfo)




