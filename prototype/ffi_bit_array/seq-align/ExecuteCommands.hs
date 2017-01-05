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
            outString = reassemble "," secondSplit
        in
        outString

-- | reassemble takes [String] and adds arg between elements and returns String
reassemble :: String -> [String] -> String
reassemble joinString inList =
    if null joinString  then error "Join string is empty"
    else if null inList then []
    else
         if not (null $ tail inList) then (head inList) ++ (joinString ++ (reassemble joinString $ tail inList))
         else head inList

-- | splitParen takes list of String and splits each one on ')' and reassmebles
splitParen :: [String] -> [String]
splitParen inList =
    if null inList then []
    else 
        let firstSplit = splitOn ")" (head inList)
            secondSplit  = splitColon firstSplit
            outList = reassemble ")" secondSplit
        in
            outList : (splitParen $ tail inList)

-- | splitColon takes list of String and splits each one on ':'deletes branch
-- length after it 
splitColon ::[String] -> [String]
splitColon inList =
    if null inList then []
    else 
        let firstSplit = splitOn ":" (head inList)
            outList = head firstSplit
        in
            outList : (splitColon $ tail inList)

{-# NOINLINE getGraphContents #-}
--getGraphContents inputs read commands, processes into data, and returns list of rawData
--uses unsafePerformIO--yuck need to remove
getGraphContents :: CommandList -> [GenForest]
getGraphContents cL =
    if null cL then []
    else  
       let (a, b) = head cL
           --fileContents = readFile (head b) -- <- readFile (head b)
           --y = unsafePerformIO fileContents
       in
       trace ("Reading graph file from " ++ show (head b)) (
       if last b == "ver" then 
           let fileContents = readFile (head b) -- <- readFile (head b)
               y = unsafePerformIO fileContents
            in
            processVertexEdgeRoot y : getGraphContents (tail cL)
       else if last b == "newick" then  --really reads extended Newick (#Hi stuff)
           let fileContents = readFile (init $ head b) -- <- readFile (head b)
               y = unsafePerformIO fileContents
               z = processNewick (removeBranchLengths y)
               t = mergeNetNodes z
            in
            [t] : getGraphContents (tail cL)
       else if last b == "fen" then 
           let fileContents = readFile (init $ head b) -- <- readFile (head b)
               y = unsafePerformIO fileContents
            in
            processFEN (removeBranchLengths y) : getGraphContents (tail cL)
       else error "File format not recognized"
       )

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
    else 
        let (a, _, _) = inNodes V.! (head) nameIndices
        in
        a : getMergeNames (tail nameIndices) inNodes

--getMergeDesc extract descList from nodes
getMergeDesc :: [Int] -> V.Vector GenPhyNetNode -> [String]
getMergeDesc nameIndices inNodes =
    if null nameIndices then []
    else 
        let (_, a, _) = inNodes V.! (head) nameIndices
        in
        a ++ getMergeDesc (tail nameIndices) inNodes

--getMergeAnc extract ancList from nodes
getMergeAnc :: [Int] -> V.Vector GenPhyNetNode -> [String]
getMergeAnc nameIndices inNodes =
    if null nameIndices then []
    else 
        let (_, _, a) = inNodes V.! (head) nameIndices
        in
        a ++ getMergeAnc (tail nameIndices) inNodes

--mergeAndPurge takes list of name occurences and 
--  if 1 time -> keep
--  if 2 times -> merge
--      purge the old nodes
mergeAndPurge :: [[Int]] -> V.Vector GenPhyNetNode -> GenPhyNet
mergeAndPurge nameTimes inNodes = 
    if null nameTimes then []
    else 
        let curNameList = head nameTimes 
            occurences = length curNameList
        in
        if occurences == 1 then (inNodes V.! (head curNameList)) : mergeAndPurge (tail nameTimes) inNodes
        else if occurences > 1 then
            let name = nub $ getMergeNames curNameList inNodes
                descList = getMergeDesc curNameList inNodes
                ancList = getMergeAnc curNameList inNodes
            in 
            if length name > 1 then error ("Problem in name list in mergeAndPurge" ++ show name)
            else 
                (head name, descList, ancList) : (mergeAndPurge (tail nameTimes) inNodes)
        else error "Problem in name list in mergeAndPurge"

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
getDividingComma newickString numLeftParens numRightParens curPosition = 
    if null newickString then (-1) --this for hitting a terminal name error "Error in binary Newick file--couldn't find comma break"
    else 
        let firstChar = head newickString
        in
        if firstChar == '(' then getDividingComma (tail newickString) (numLeftParens + 1) numRightParens (curPosition + 1)
        else if firstChar == ')' then getDividingComma (tail newickString) numLeftParens (numRightParens + 1) (curPosition + 1)
        else if firstChar == ',' then 
            if numLeftParens == numRightParens then curPosition
            else getDividingComma (tail newickString) numLeftParens numRightParens (curPosition + 1)
        else getDividingComma (tail newickString) numLeftParens numRightParens (curPosition + 1)

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
    else 
        takeWhile (/= ':') inLabel

--getRestNewickle takes from the root node set in processnewick and recurses
--through the subtrees until only terminals are found (no commas)
getRestNewick :: String -> String -> GenPhyNet
getRestNewick inSubTree ancTree =
    --trace ("\nGRN inSubTree: " ++ inSubTree ++ " \nanc " ++ ancTree) (
    if null inSubTree then error ("Error in Newick file parsing.  Anc: " ++ ancTree)
    else 
        if (notElem ',' inSubTree) then --commaPosition == maxIntLocal then
            --trace ("\nTerminal " ++ show ancTree ++ "->" ++ show inSubTree) (
            if (head inSubTree /= '(') then [(inSubTree, [], [ancTree])]
            else --is indegree=outdegree=1 node 
                let nodeName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse inSubTree
                    descNode = reverse $ dropWhile (/= ')') $ reverse inSubTree
                    descNodeName = stripLabel $ tail $ init descNode --getSisters descNode  --tail $ init $ descNode
                in
                --trace ("\n11 " ++ show inSubTree ++ " " ++ show nodeName ++ " " ++ show descNode ++ " " ++ show descNodeName) (
                if null nodeName then
                    [(inSubTree, [descNodeName], [ancTree])] ++ (getRestNewick descNodeName inSubTree)
                else 
                     [(nodeName, [descNodeName], [ancTree])] ++ (getRestNewick descNodeName nodeName)
                --)
        else 
            --set node and recurse
            let nodeName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse inSubTree
                subTreeStripped =  reverse $ dropWhile (/= ')') $ reverse inSubTree
                subTreeNoLabel = tail $ init $ subTreeStripped --getSisters subTreeStripped --tail $ init $ subTreeStripped
                commaPositionHere = getDividingComma subTreeNoLabel 0 0 0
                (leftDesc, preRightDesc) = splitAt commaPositionHere subTreeNoLabel --assumes binary
                rightDesc = dichotomize (tail preRightDesc) --removes ',' and resolves polytomies
                leftDescStripped = stripLabel leftDesc
                rightDescStripped = stripLabel rightDesc 
            in
            {-trace ("\n label stuff " ++ show nodeName ++ " " ++ show subTreeStripped ++ " " ++ show subTreeNoLabel 
                ++ " " ++ show commaPositionHere ++ "\nInternal " ++ show ancTree ++ "->" ++ show leftDesc ++ " and " 
                ++ show rightDesc ++ " label " ++ show nodeName ++ show (getDividingComma rightDesc 0 0 0)) (
            -}
            --trace ("posLR " ++ show commaPositionHere ++ " reverse " ++ show ((length subTreeNoLabel) - (getDividingComma (reverse subTreeNoLabel) 0 0 0) - 1) ++ show rightDesc) (
            if null nodeName then
                [(inSubTree, [leftDescStripped, rightDescStripped], [ancTree])] ++ (getRestNewick leftDesc inSubTree ) 
                    ++ (getRestNewick rightDesc inSubTree)
            else 
                [(nodeName, [leftDescStripped, rightDescStripped], [ancTree])] ++ (getRestNewick leftDesc nodeName) 
                    ++ (getRestNewick rightDesc nodeName)
      --      )

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
    --check pren numbers match
    let newickString1 = filter (/= '\n') inNewickString --remove newlines
        newickString = filter (/= ' ') newickString1 --remove spaces
        leftParens = length $ elemIndices '(' newickString
        rightParens = length $ elemIndices ')' newickString
    in
    if leftParens /= rightParens then error ("Parentheses do not match in Newick input" ++ show newickString)
    else if not ((head newickString == '(') && (last newickString == ';')) then error "Newick tree must start with '(' and end with ';'"
    else 
        let gutsOnlyLabel = takeWhile (/= '[') $ init newickString --disgards comments after here, assumes only comment at end before ';'
            nodeName = stripBranchLengths $ reverse $ takeWhile (/= ')') $ reverse gutsOnlyLabel
            gutsOnly = reverse $ dropWhile (/= ')') $ reverse gutsOnlyLabel
            nodeAnc = []
            reducedString = tail $ init gutsOnly --getSisters gutsOnly --tail $ init gutsOnly
            commaPosition = getDividingComma reducedString 0 0 0
        in
        if commaPosition /= (-1) then 
            let (leftDesc, preRightDesc) = splitAt commaPosition reducedString --assumes binary
                rightDesc = tail preRightDesc --removes ','
                leftDescStripped = stripLabel leftDesc
                rightDescStripped = stripLabel rightDesc --check for polytomies here
            in
            {-trace ("\nRoot label = " ++ show nodeName ++ "\n processed newick " ++ show reducedString ++ "->" 
                ++ show leftDesc ++ " " ++ show (tail rightDesc)) (
            -}
            --trace ("posLR " ++ show (getDividingComma rightDesc 0 0 0) ++ show rightDesc) (
            if null nodeName then
                (gutsOnly, [leftDescStripped, rightDescStripped], []) :  ((getRestNewick leftDesc gutsOnly ) ++ (getRestNewick rightDesc gutsOnly)) 
            else (nodeName, [leftDescStripped, rightDescStripped], []) :  ((getRestNewick leftDesc nodeName ) ++ (getRestNewick rightDesc nodeName))
            --)
        else --single taxon tree
            if null nodeName then (gutsOnly, [stripBranchLengths reducedString], []) : (getRestNewick reducedString gutsOnly)
            else (nodeName, [stripBranchLengths reducedString], []) : (getRestNewick reducedString nodeName)

--processNewickComponents takes list of parse newicks from fen file,
--add trailing ';' and processes newicks, returnin a forest
processNewickComponents :: [String] -> GenForest
processNewickComponents inNewickList =
    if null inNewickList then []
    else 
        let componentString = (head inNewickList) ++ ";" --to make valis newick after endby ";"
            newickNodes = processNewick componentString
            enhancedNewickNodes = mergeNetNodes newickNodes
        in
        --trace ("\nNewick Components " ++ show newickNodes) 
        enhancedNewickNodes : (processNewickComponents (tail inNewickList))

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
getReadContents cL =
    if null cL then []
    else  
       let (a, b) = head cL
       in --check for file type here
       if (length b == 1) then
            let fileContents = readFile (head b) -- <- readFile (head b)
                y = unsafePerformIO fileContents
            in
            if head y == '>' then processFastaInput y :  getReadContents (tail cL) --(processFileList (tail x)))
            else if (head y == 'X') || (head y == 'x') then processTNTInput y :  getReadContents (tail cL) --(processFileList (tail x))) 
            else error "Unrecognized input file format."
       else if (length b == 2) then --there is a file type argument
            let filetype = last b --assumes only two arguments
            in
            trace ("\n " ++ show b ++ "\n") (
            if filetype == "custom_alphabet" then 
                let fileName = takeWhile (/= '\"') (head b)
                    fileContents = readFile fileName
                    fileStuff = unsafePerformIO fileContents
                    secondPart = takeWhile (/= '\"') $ tail $ dropWhile (/= '\"') $ dropWhile (/= ',') (head b) 
                    tcmFileStuff = unsafePerformIO $ readFile secondPart
                in
                trace ("\ntcmFile " ++ show secondPart)
                processCustomAlphabet fileStuff tcmFileStuff :  getReadContents (tail cL) 
            else error "Unrecognized/implemented input filetype"
            )
       else if (length b > 2) then error "too many file arguments in read command"
       else error "Error in command line parse."

--getTermDataList extracts List of List of TermData for 
--compilation
getTermDataList :: [RawData] -> [[TermData]]
getTermDataList x = 
    if null x then []
    else 
        let termDataList = fst $ head x
        in
        termDataList : getTermDataList (tail x)

--getNameData for given name get data Strings
--if not found then add "no_data" missing data String
getNameData :: String -> [[TermData]] -> [String]
getNameData curName termDataListList =
    if null termDataListList then []
    else
        let retrievedStuff = lookup curName (head termDataListList)
        in
        if isNothing retrievedStuff then "no_data" : getNameData curName (tail termDataListList)
        else fromJust (lookup curName (head termDataListList)) ++ 
                getNameData curName (tail termDataListList)

--extractDataByName does through list of term info and extracts data ([String])
--and cons to existing--by name
extractDataByName :: [String] -> [[TermData]] -> [TermData]
extractDataByName searchNameList termDataListList =
    if null searchNameList then []
    else 
        let searchName = head searchNameList
            nameData =  getNameData searchName termDataListList 
            allTermData = (searchName, nameData) : extractDataByName (tail searchNameList) termDataListList
        in
        allTermData

--getCharInfo extracts and concats character Inof list from RawData List
getCharInfo :: [RawData] -> [CharInfo]
getCharInfo x =
    if null x then []
    else 
        let (baseTermData, baseCharInfo) = head x
            newCharInfo = baseCharInfo ++ getCharInfo (tail x)
        in
        --trace ("elided " ++ show (length newCharInfo) ++ " charInfo's")
        newCharInfo

--getNameVect extracts terminals names from RawData
getNameList :: [TermData] -> [String]
getNameList x = 
    if null x then []
    else 
        let (newName , _) = head x
            nameList = newName : getNameList (tail x)
        in
        --trace ("Name list has " ++ show (length nameList) ++ " entries")
        nameList 

--getNameListAll gets total name list over all inputs--helps with missing data
--etc in multiple input files
getNameListAll :: [[TermData]] -> [String]
getNameListAll x =
    if null x then []
    else 
        getNameList (head x) ++ getNameListAll (tail x)

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




