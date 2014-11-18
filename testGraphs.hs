module Main where
import System.Environment
import Debug.Trace
import Parsers.ReadGraphs

-- | 'main' Main function to run program 
main :: IO ()
main = 
  do 
    --Get process VertexEdgeRoot Forest format
    allFileData <- getContents
    let inputGraph = processVertexEdgeRoot allFileData 
    print ("There are " ++ (show (length inputGraph)) ++ " tree(s) in input graph.")
    print ("Tree : " ++ (show (inputGraph !! 0)))
    print ("Tree : " ++ (show (inputGraph !! 1)))
    --print ("Char info " ++ (show info))
    --print ("Data " ++ (show pairedData))
    --printInputData pairedDataa
    printGraph inputGraph
    printGraphVizDot inputGraph "tempFile.dot"
    --printInputDataByCharacter pairedData
