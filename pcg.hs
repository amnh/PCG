{- |
Module      :  Phylogenetic Component Graph (PCG)
Description :  Top level program for Phylogenetic Component Graph (PCG) 
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

module Main where

import System.IO
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as Set
import CharacterData
import ReadFiles
import ReadGraphs
import ProcessCommands
import ExecuteCommands
import Component

-- | 'main' Main Function to run PCG
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        scriptFileHandle <- checkScriptInfo args
        commandDataString <- hGetContents scriptFileHandle
        let commandList = reverse $ parseCommandList (T.pack commandDataString)
        hPutStrLn stderr ("Command list " ++ show commandList)
        let (readDataList, readGraphList, reportList, exitList, analysisList) = parseCommands commandDataString 
        let pairDataList = getReadContents readDataList
        let inputGraphList = getGraphContents readGraphList
        if not $ null inputGraphList then printGraphVizDot (head inputGraphList) "tempFile.dot"
        else hPutStrLn stderr "No input graph to report"
        hPutStrLn stderr ("There are " ++ show (length pairDataList) ++ " input data files.")
        hPutStrLn stderr ("There are " ++ show (length inputGraphList) ++ " input graph files.")
        let inputData  = flattenCharList pairDataList  
        let termNameList = getNameList $ fst inputData
        --Check that leaves on graph and input data agree
        let graphAndDataJibe = checkGraphAndData (Set.fromList termNameList) inputGraphList
        if graphAndDataJibe then hPutStrLn stderr "Input graphs and data jibe"
        else error "Input graphs and data terminals are inconsistent"
        hPutStrLn stderr ("There are " ++ show (length $ fst inputData) ++ 
            " taxa and " ++ show (length $ snd inputData) ++ " characters")
        --Check for cycles in input graphs
        {-
        let checkCycles = areCycles inputGraphList
        if checkCycles then error "There are cycles in input graphs, which are not allowed"
        else hPutStrLn stderr "Inp[ut graphs are acyclic."
        -}
        --printInputData $ fst inputData
        let phyloData = createBaseData inputData
        let newCharInfo = redoRootCosts phyloData (snd inputData) 0
        --printDataMatrixVLS phyloData termNameList
        let curForestList = baseDataToLeafNodes inputGraphList
        hPutStrLn stderr ("Data recoded " ++ show (V.length phyloData) ++ " leaves in " 
            ++ show (length $ head curForestList) ++ " components" ++ " with root cost " 
            ++ show (getRootCosts newCharInfo))
        --hPrint stderr curForestList
        let inCost = getForestCostList phyloData curForestList newCharInfo termNameList
        hPutStrLn stderr ("At cost " ++ show (head inCost))
