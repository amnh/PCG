{- |
Module      :  Command process functions
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

module ProcessCommands
( Command
, CommandList
, parseCommands
, parseCommandList
, checkScriptInfo
) where

import System.IO
import System.Process
import System.Environment
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Text as T

type Command = (String, [String])
type CommandList = [Command]

permissibleCommands = ["read", "exit", "build"] 
maxInt = 1000000000

--checkScriptInfo takes command line and verifies that there is a single
--script file of commands specified
checkScriptInfo :: [String] -> IO Handle
checkScriptInfo inArgs
    | null inArgs = error "Must specify an input script file."
    | length inArgs > 1 =
      error "Must specify a SINGLE input script file."
    | otherwise =
      do putStrLn "Input script:"
         putStr (head inArgs)
         openFile (head inArgs) ReadMode

-- | getLastCommand take a list of String commands and a Text type list 
-- and determines the last of the commands to parese back to front by parse
-- commands
getLastCommand :: [String] -> T.Text -> Int -> T.Text -> T.Text
getLastCommand c x best bestText =
    if T.null x then error "Command unrecognized"
    else if null c then bestText
    else 
        let (a, b) = T.breakOnEnd (T.pack $ head c) x
        in
        if T.length b < best then
            getLastCommand (tail c) x  (T.length b) (T.pack $ head c) 
        else 
            getLastCommand (tail c) x best bestText

-- | parseCommandList splits by permissible commands
parseCommandList :: T.Text -> [(T.Text,T.Text)]
parseCommandList x = 
    if T.length x == 0 then []
    else
        --trace ("Parsing " ++ show x) (
        let lastCommand = getLastCommand permissibleCommands x maxInt T.empty
            (a, b) = T.breakOnEnd lastCommand x
            toDrop = length $ head permissibleCommands
            c = T.take ((T.length a) - toDrop) a
        in
        (lastCommand, b) : (parseCommandList c)
        

-- | parseCommands parses lines of input file or perhaps interactive 
--to get program options
--for now--this is simeple and sucks
--only one command per line for now--also sucks
--need to split commands into "read" "process" "report"
--"exit" (to allow for potential interactive interface)
--to deal with IO issues--also also sucks
parseCommands :: String -> (CommandList, CommandList, CommandList, CommandList, CommandList) 
parseCommands y =
    if null y then error "Empty command string."
    else
        --recursive call to get commands and args
        let x = filter (/= ' ') y
            allCommands = getCommandList (lines x)
            readCommands = filterCommandList allCommands "read"
            reportCommands = filterCommandList allCommands "report"
            exitCommands = filterCommandList allCommands "exit"
            readGraphCommands =  getGraphReads readCommands
            analysisCommands = (((allCommands \\ readCommands) \\ reportCommands) \\ exitCommands)
        in
        (readCommands \\ readGraphCommands, readGraphCommands, reportCommands, exitCommands, analysisCommands)

--getCommandList recursively takes string list and concats with 
--growing list of Commands
getCommandList ::  [String] -> CommandList 
getCommandList y 
    | null y = []
    | head y == "" = []
    | otherwise =
      let x = head y
          command = takeWhile (/= '(') x
          rest = dropWhile (/= '(') x
          endCommand = elemIndices ')' rest
          argumentList = drop 1 (take (last endCommand - 1) rest)
        in
        if null endCommand then
          error ("Improperly formatted command" ++ show command) else
          if head argumentList == '"' then
            let argument = drop 2 (take (last endCommand - 1) rest) in
                (command, [argument]) : getCommandList (tail y)
          else 
            let fileType = takeWhile (/= ':') argumentList
                fileName = drop 1 $ dropWhile (/= '"') argumentList in
                trace (show command ++ show fileName ++ show fileType)
                (command, [fileName, fileType]) : getCommandList (tail y)

--getGraphReads pulls out graph read commands form all read commands
getGraphReads :: CommandList -> CommandList
getGraphReads x =
    if null x then []
    else 
        let (a, arguments) = head x
        in
        if length arguments == 1 then getGraphReads (tail x)
        else if arguments !! 1 == "ver" then 
            (a, arguments) : getGraphReads (tail x)
        else if arguments !! 1 == "fen" then 
            (a, arguments) : getGraphReads (tail x)
        else if arguments !! 1 == "newick" then 
            (a, arguments) : getGraphReads (tail x)
        else getGraphReads (tail x) 
                
--filterCommandList takes command and filters out those commands from overall
--list that do not match input command string
filterCommandList :: CommandList -> String -> CommandList
filterCommandList xList yCommand
    | null xList = []
    | null yCommand = error "command String empty"
    | otherwise =
      let (a, b) = head xList in
        if a == yCommand then
          (a, b) : filterCommandList (tail xList) yCommand else
          filterCommandList (tail xList) yCommand

