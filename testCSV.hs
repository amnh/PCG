module Main where
import System.Environment
import Debug.Trace
import ReadFiles

-- | 'main' Main function to run program 
main :: IO ()
main = 
  do 
    --Get data and process fasta format
    allFileData <- getContents
    let (pairedData, info) = processCsvInput allFileData 
    print ("There are " ++ (show (length pairedData)) ++ " taxa and " ++ (show (length info)) ++ " characters.")
    --print ("Char info " ++ (show info))
    --print ("Data " ++ (show pairedData))
    --printInputData pairedDataa
    printInputDataByTerminal pairedData
    --printInputDataByCharacter pairedData
