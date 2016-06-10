module Main (main) where

import Data.Vector
import Data.Word
import SimpleGPUTest
import Safe
import System.Environment (getArgs)

main :: IO ()
main = getArgs
   >>= processArgs


processArgs :: [String] -> IO ()
processArgs      [] = print test1
processArgs     [x] = putStrLn "Only one list supplied... give me one more!"
processArgs (x:y:_) =
  case readSeqs x y of
    Nothing        -> putStrLn "Could not read sequences!"
    Just (lhs,rhs) -> print $ simpleTest (fromList lhs) (fromList rhs)

readSeqs x y = do
  lhs <- readMay x :: Maybe [Word16]
  rhs <- readMay y :: Maybe [Word16]
  pure (lhs, rhs)
