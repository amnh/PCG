{- |
Module      :  Functions for reading file formats (fasta, nexus, TNT,csv)
Description :  Reads input fasta file and returns lsit of pairs with taxon name and sequence 
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

--TO DO--read file control words and lower case with conversions.

module ReadFiles
( processFastaInput
, processCustomAlphabet
--, processCustomAlphabetMatrix
, processTNTInput
, processNexusInput
, processCsvInput
, printInputData
, printInputDataByTerminal
, printInputDataByCharacter
, CharType(..)
, CharInfo(..)
, RawData
, TermData
, modifyRootCost
) where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import Debug.Trace

-- | CharType data type for input characters
data CharType = Add | NonAdd | Matrix | NucSeq | AminoSeq | GenSeq | Genome
    deriving (Read, Show, Eq)

-- | CharInfo information about characters
-- need to add rootCost for forest optimization
data CharInfo = CharInfo { charType :: CharType
                         , activity :: Bool
                         , weight :: Float
                         , costMatrix :: [Int]
                         , name :: String
                         , numStates :: Int
                         , alphabet :: [String]
                         , rootCost :: Float
                         } deriving (Show, Eq)

-- | RawData type processed from input to be passed to characterData
--to recode into usable form
--the format is tuple of a list of taxon-data list tuples and charinfo list.
--the data list and charinfo list must have the same length
type RawData = ([TermData], [CharInfo])

-- | type TermData type contians termnal name and list of characters
type TermData = (String, [String])


--Funtions to modify char info values
modifyCharType :: CharInfo -> CharType -> CharInfo
modifyCharType charState x =
    charState { charType = x }

modifyActivity :: CharInfo -> Bool -> CharInfo
modifyActivity charState x =
    charState { activity = x }

modifyWeight :: CharInfo -> Float -> CharInfo
modifyWeight charState x =
    charState { weight = x }

modifyCostMatrix :: CharInfo -> [Int] -> CharInfo
modifyCostMatrix charState x =
    charState { costMatrix = x }

modifyName :: CharInfo -> String -> CharInfo
modifyName charState x =
    charState { name = x }

modifyNumStates :: CharInfo -> Int -> CharInfo
modifyNumStates charState x =
    charState { numStates = x }

modifyRootCost :: CharInfo -> Float -> CharInfo
modifyRootCost  charState x =
    charState { rootCost = x }

-- | getTaxonNames identify taxon names (with leading '>'--no space after '>') from input lines (to allow for
--non-name info in taxon line ala fasta
getTaxonNames :: [String] -> [String]
getTaxonNames y
    | null y = []
    | null (head y)  = getTaxonNames (tail y)
    | otherwise =
      let x = head y
          xs = tail y
        in
        if head x /= '>' then getTaxonNames xs else
          let wx = words x
              hwx = head wx
              thwx = tail hwx
            in thwx : getTaxonNames xs

-- | getSequences get list for each taxon (names and sequence) by checking if word is name
--and if not appending to that list
getSequences :: [String] -> [String]
getSequences [] = []
getSequences (x:xs)
    | null (x:xs) = []
    | null x = getSequences xs
    | head x  == '>' = ">" : getSequences xs
    | otherwise = concat (words x) :  getSequences xs 

-- | getCustomSequences
--get list for each taxon (names and sequence) by checking if word is name
--and if not appending to that list--but leave spaces between symbols
getCustomSequences :: [String] -> [String]
getCustomSequences [] = []
getCustomSequences (x:xs)
    | null (x:xs) = []
    | null x = getCustomSequences xs
    | head x  == '>' = ">" : getCustomSequences xs
    | otherwise = x :  getCustomSequences xs 

-- | mergeSeqs
--merge list of String into String
mergeSeqs :: [[String]] -> [String]
mergeSeqs [] = []
mergeSeqs x = map concat x  

-- | convertSeqToList makes sequence into list of String to move towards rawData
--type
convertSeqToList :: [(String, String)] -> [(String, [String])]
convertSeqToList x =
    if null x then []
    else 
        let (y, z) = head x
        in
            (y, [z]) : convertSeqToList (tail x)

-- | processFastaInput
--Process input fasta data and return RAwData--but only for asingle input file
--Taxon names must begin with '>' with no spaces, charcaters after spaces on 
--  taxon line are ignores (e.g. genbank #s) 
--does not filter out numbers fomr sequences to allow generality, spaces in sequences
--shouldn't matter
--now does 2 passes + zip, but should be made into a single pass
--breaking on names with '>' only 
processFastaInput :: String -> RawData 
processFastaInput x = 
    if  null x then ([],[])
    else 
        let inLines = lines x
            taxNames = getTaxonNames inLines
            taxSeqs = getSequences inLines
            groupSeqs =  tail (splitOn [">"] taxSeqs)
            contSeqs = mergeSeqs groupSeqs
            pairedData = zip taxNames contSeqs
            pairedListData = convertSeqToList pairedData

        in 
        let defaultFastaCharInfo = CharInfo {
                                   charType = NucSeq
                                 , activity = True
                                 , weight = 1.0
                                 , costMatrix = []
                                 , name = "FastaSeqChar"
                                 , numStates = 0
                                 , alphabet = ["A", "C", "G", "T", "-"]
                                 , rootCost = 0.5
                                 }
        in
            (pairedListData, [defaultFastaCharInfo])

-- | getInts takes String andretuns [Int] 
getInts :: [String] -> [Int]
getInts inString =
    if null inString then []
    else 
        let a = head inString
            b = read a :: Int
        in
        b : getInts (tail inString)


-- | processTCM takes tcmfile contents and returns alphabet and 
--costmatrix
processTCM :: String -> ([String], [Int])
processTCM tcmStuff =
    if null tcmStuff then error "tcm file empty"
    else
        let firstLine = head (lines tcmStuff)
            alphabet = words firstLine
            costMatrix = getInts $ tail (lines tcmStuff) --not really processed now
        in
        (alphabet, costMatrix)

-- | processCustomAlphabet takes input custom_alphabet sequences 
--and returns processed data
processCustomAlphabet :: String -> String -> RawData 
processCustomAlphabet x tcmStuff = 
    if  null x then ([],[])
    else 
        let inLines = lines x
            taxNames = getTaxonNames inLines
            taxSeqs = getCustomSequences inLines
            groupSeqs = tail (splitOn [">"] taxSeqs)
            contSeqs = mergeSeqs groupSeqs
            pairedData = zip taxNames contSeqs
            pairedListData = convertSeqToList pairedData
            (alphabetList , matrixList) = processTCM tcmStuff
        in 
        let defaultGenSeqCharInfo = CharInfo {
                                   charType = GenSeq
                                 , activity = True
                                 , weight = 1.0
                                 , costMatrix = matrixList
                                 , name = "CustomAlphabetChar"
                                 , numStates = 0
                                 , alphabet = alphabetList
                                 , rootCost = 0.5
                                 }
        in
            if (length alphabetList) > 63 then error "Alphabet > 63"
            else 
                trace ("\nCA alphabet " ++ show (alphabetList) )-- ++ " " ++ show matrixList)
                (pairedListData, [defaultGenSeqCharInfo])

-- | reformatCharString
--Convert String of chars into list of String so a charcater can be a sequence,
--or ambiguous if Hennig86/TNTR/Nexus
reformatCharString :: String -> [String]
reformatCharString x
    | null x = []
    | head x /= '[' = [head x] : reformatCharString (tail x)
    | otherwise =
      let y = splitOn "]" (tail x) in
        trace ("y = " ++ show y)
          (head y : reformatCharString (concat (tail y)))

-- | extractLines Extract pairs form lines of data file body
extractLines :: [String] -> [(String, [String])]
extractLines [] = []
extractLines (x:xs) = (x, reformatCharString (head xs)) : extractLines (tail xs) 

-- | getTaxCharPairs
--get Taxon, Characters Pairs assuming on same line separated by space
getTaxCharPairs :: String -> [(String, [String])]
getTaxCharPairs x =
    if null x 
        then []
    else 
        let guts = words x
            resPairs = extractLines guts
        in
            --trace ("in " ++ (show x) ++  "to " ++ (show guts) ++ " to " ++ (show resPairs)) 
            resPairs

-- | getAction returns charcater type info from Hennig86/TNT +,-,[,],w etc
getAction :: Char -> String
getAction x
    | x == '-' = "NonAdditive"
    | x == '+' = "Additive"
    | x == '[' = "Active"
    | x == ']' = "InActive"
    | x == '/' = "Weight"
    | otherwise = error ("Unrecognized character attribute " ++ show x ++ " in Hennig86/TNT input file.")

-- | stringToInt Converts sString to Int, needs to return or check if not an Int.
stringToInt :: String -> Int
stringToInt x =
    if null x then error ("Input error " ++ show x ++ "in stringToInt function")
    else 
        let y = read x :: Int
        in
        y
        
-- | getScope reads Hennig86/TNT scope options '.' etc and sets
getScope :: [String] -> Int -> [Int]
getScope (x : xs) nchar
    | null (x : xs) =
      error "Missing character scope in Hennig86/TNT input file."
    | length (x : xs) == 2 =
      let y = words x
          z = words (head xs)
        in
        if null y then
          if null z then [0 .. (nchar - 1)] else
            let second = stringToInt (head z) in [0 .. second]
          else
          let first = stringToInt (head y) in
            if null z then [first, nchar - 1] else
              let second = stringToInt (head z) in [first .. second]
    | length (x : xs) == 1 =
      let individualNums = map stringToInt (words x) in individualNums
    | otherwise =
      error
        ("Unrecognized character scope " ++
           show (x : xs) ++ " in Hennig86/TNT input file.")

-- | processWeightScope removes and returns weight adnd scope as separate theings
processWeightScope :: String -> String -> (String, Int)
processWeightScope attribute x  
    | null x = ([], 0)
    | attribute /= "Weight" = (x, 1)
    | otherwise =
      let y = words x
          weight = stringToInt (head y)
          z = unwords (tail y)      
        in (z, weight)

-- | modifyCharInfo updates CharInfo record, overwriting previous values
--checks to make sure modifucations are within nchar
modifyCharInfo :: [CharInfo] -> String -> [Int] -> Int -> Int -> [CharInfo]
modifyCharInfo oldCharInfo attribute scope weight nchar =
    if null scope || (head scope > (nchar - 1)) || (head scope < 0) then oldCharInfo    
    else 
        let (xs, ys) = splitAt (head scope) oldCharInfo
        in
        if  attribute == "NonAdditive" then
            let newCharElem = modifyCharType (oldCharInfo !! head scope) NonAdd
                newCharInfo = xs ++ [newCharElem] ++ tail ys
            in
            modifyCharInfo newCharInfo attribute (tail scope) weight nchar
        else if attribute == "Additive" then
            let newCharElem = modifyCharType (oldCharInfo !! head scope) Add
                newCharInfo = xs ++ [newCharElem] ++ tail ys
            in
            modifyCharInfo newCharInfo attribute (tail scope) weight nchar

        else if attribute == "Active" then
            let newCharElem = modifyActivity (oldCharInfo !! head scope) True
                newCharInfo = xs ++ [newCharElem] ++ tail ys
            in
            modifyCharInfo newCharInfo attribute (tail scope) weight nchar
        else if attribute == "InActive" then
            let newCharElem = modifyActivity (oldCharInfo !! head scope) False
                newCharInfo = xs ++ [newCharElem] ++ tail ys
            in
            modifyCharInfo newCharInfo attribute (tail scope) weight nchar
        else if attribute == "Weight" then
            let newCharElem = modifyWeight (oldCharInfo !! head scope) (fromIntegral weight)
                newCharInfo = xs ++ [newCharElem] ++ tail ys
            in
            modifyCharInfo newCharInfo attribute (tail scope) weight nchar
        else  error ("Unrecognized character attribute " ++ show attribute ++ " in input file.")
    
-- | setCodes based on Henig/TNT get and set action and scope 
setCodes :: [String] -> Int -> [CharInfo] -> [CharInfo]
setCodes x nchar initialCharInfo =
    if null x then []
    else 
        let 
            y = unwords x
            attribute = getAction (head y)
            (z, weight) = processWeightScope attribute (tail y)
            scope = getScope (splitOn "." z) nchar
            newCharInfo = modifyCharInfo initialCharInfo attribute scope weight nchar
        in
            --trace ("parsing " ++ (show y) ++ "atr " ++ (show attribute) ++ "prescope " 
            -- ++ (show (tail y)) ++ " sc " ++ (show (splitOn "." (tail y))) ++ " scope " ++ (show scope)) 
            newCharInfo

-- | getCharInfo information on charcater type etc from lines after data body--one set
--option (cc -.; cc[ 0;) per semicolon
getCharInfo :: [String] -> Int -> [CharInfo] -> [CharInfo]
getCharInfo (x : xs) nchar initialCharInfo
    | null (x : xs) = []
    | head (words x) == "proc" || head (words x) == "proc/" =
      initialCharInfo
    | head (words x) == "cc" || head (words x) == "ccode" =
      let newCharInfo = setCodes (tail (words x)) nchar initialCharInfo
          newerCharInfo = getCharInfo xs nchar newCharInfo
        in newerCharInfo
    | otherwise = error ("Unrecongized character option " ++ show x)

-- | initializeCharInfo sets chrcater info to defaults 
initializeCharInfo :: CharInfo -> Int -> Int -> [CharInfo]
initializeCharInfo defaults nchar acc =
    if acc == nchar then []
    else 
        let newChar = CharInfo {
                          charType = charType defaults
                        , activity = activity defaults
                        , weight = weight defaults
                        , costMatrix = costMatrix defaults
                        , name = name defaults
                        , numStates = numStates defaults
                        , alphabet = []
                        , rootCost = 0.5
                        }
        in
        newChar : initializeCharInfo defaults nchar (acc + 1)

-- | parse Xread version of Hennig86/TNT file
processXread :: [String] -> RawData 
processXread x = 
    if null x 
        then ([], [])
    else 
        let defaultHennigCharInfo = CharInfo {
                          charType = Add
                        , activity = True
                        , weight = 1.0
                        , costMatrix = []
                        , name = "Hennig86/TNTChar"
                        , numStates = 0
                        , alphabet = []
                        , rootCost = 0.5
                        }
            thang = splitOn "'" (unwords x)
            message = head (tail thang)
            body = words (head (tail (tail thang)))
            nchar = stringToInt (head body)
            initialCharInfo = initializeCharInfo defaultHennigCharInfo nchar 0
            ntax = stringToInt (head (tail body))
            rest = splitOn ";" (unwords (tail (tail body)))
            taxCharPair = getTaxCharPairs (head rest) 
            charInfo = getCharInfo (tail rest) nchar initialCharInfo
        in
            trace ("Hennig/TNT file message " ++ show message ++ " ntax = " ++ show ntax ++ " nchar = " ++ show nchar) 
            (taxCharPair, charInfo)

-- | processTNTInput
--Process basic Hennig86/TNT file
--assumes:
--1) starts with "xread"
--2) follwed by a mandatory comment in single quotes -- 'blah'  (Need to make
--  this optional later)
--3) line with nchar and ntax
--4) lines of taxname , space, characters with no breaks and single char states
--  or ambiguous in square brackets == [01]
--5) semi colon at end of character info ';'
--6) cc or ccode commands one per line terminated with semicolon ';' 
--7) scopes as '.' 'a.' '.b' a.b' or 'a b'
--8) last line 'proc /;' 
--Not yet implemented
--  1) tread
--  2) costmatrix
processTNTInput :: String -> RawData  
processTNTInput x =
    if null x 
        then ([], [])
    else 
        let header = head (words x) in
        if T.unpack (T.toLower (T.pack header)) == "xread" 
            then processXread (tail (words x))
        else error "Only processing 'xread' Hennig/TNT files for now"

-- | processNexusInput
--Process basic Nexus file (eventually complete to specification)
processNexusInput :: String -> RawData
processNexusInput x =
    error (if null x then error "NEXUS stream empty."
    else "NEXUS file parsing not implemented.") --([],[])

-- | assignCharType
--assignType makes a character info array of itype 'y' for each column
--in 'x' file
assignCharType  :: Int -> CharInfo -> [CharInfo]
assignCharType x y 
    | x == 0 = []
    | otherwise = y : assignCharType (x - 1) y 

-- | getPairDataCSV
--Process CSV input as lines to extract name and data cells
getPairDataCSV :: [String] -> [(String, [String])]
getPairDataCSV x =
    if null x then []
    else 
        let currentLine = splitOn "," (head x)
            name = head currentLine
            characters = tail currentLine
        in
        if (name == "\n") || (name == "\r") || null characters then getPairDataCSV (tail x)
        else 
            trace ("CSV in " ++ show name ++ " " ++ show (length x) ++ " " ++ show (length characters)) (name, characters) : getPairDataCSV (tail x)
 
-- | processCsvInput Process CSV ',' delimited file
--assumes
--One taxon per line first field taxon name
--  comma delimited cells in lines, one line per terminal
processCsvInput :: String -> RawData 
processCsvInput x =
    if null x 
        then error "CSV stream empty."
    else
        let inLines = split (oneOf "\n\r") x
            pairedNameData = getPairDataCSV inLines
            (_, charData) = head pairedNameData
            defaultCsvInfo = CharInfo {
                       charType = GenSeq
                     , activity = True
                     , weight = 1.0
                     , costMatrix = []
                     , name = "CsvChar"
                     , numStates = 0
                     , alphabet = []
                     , rootCost = 0.5
                     }
            charTypeValue = assignCharType (length charData) defaultCsvInfo 
        in
        trace ("lines in " ++ show (length inLines)) 
        (pairedNameData, charTypeValue)

-- | printStrinListWithNewLine between elements
printStringListWithNewLine :: [String] -> IO ()
printStringListWithNewLine x =
    if null x then putStrLn " \\newpage "
    else 
        do
            putStr (head x)
            putStrLn " \\\\ "
            printStringListWithNewLine (tail x)

-- | printInputCharacters
--Print Characters 
printInputCharacters :: [String] -> IO ()
printInputCharacters x =
    if not (null x) then
        do 
            putStr (head x)
            putStr " "
            printInputCharacters (tail x)
    else putStrLn ""

-- | printInputData
--Print input data as strings to check validity of the read
--uses the ``pairedNameData'' one line per taxon, taxon
--name followed by characters then \n
printInputData :: [(String, [String])] -> IO ()
printInputData x = 
    if not (null x) then 
        do
          let (a,b) = head x
          putStr a
          putStr " "
          printInputCharacters b
          printInputData (tail x)
    else putStrLn ""

-- | printInputDataByTerminal
--Print input data as strings to check validity of IPA read
--uses the ``pairedNameData'' one page per taxon
--characters in column including latex commands for IPA
printInputDataByTerminal :: [(String, [String])] -> IO ()
printInputDataByTerminal x = 
    if not (null x) then 
        do
          let (a,b) = head x
          putStrLn "\\end{IPA}"
          putStr a
          putStrLn "\\begin{IPA}"
          putStr "\\\\"
          printStringListWithNewLine b
          printInputDataByTerminal (tail x)
    else putStrLn ""

-- | removeTerminalNames
--Remove the names from pairedData
removeTerminalNames ::  [(String, [String])] -> [[String]]
removeTerminalNames x =
    if null x then []
    else 
        let (a, b) = head x
        in
        b : removeTerminalNames (tail x)

-- | printOneAtATime
--Print one char at a time
printOneAtATime :: [[String]] -> IO ()
printOneAtATime x = 
    if null x then putStrLn ""
    else 
        do
            printStringListWithNewLine (head x)
            printOneAtATime (tail x)

-- | printInputDataByCharacter
--Print input data as columns of character states 1 at a time then \n
--so get a single column output separated by new line latex characters
printInputDataByCharacter :: [(String, [String])] -> IO ()
printInputDataByCharacter x = 
    if not (null x) then
        do
            let y = removeTerminalNames x
            let z = transpose y
            printOneAtATime z
    else putStrLn ""
        




