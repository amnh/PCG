module Main (main) where

import Data.Vector        (fromList)
import Data.Word
import Safe
import SimpleGPUTest
import System.Environment (getArgs)

main :: IO ()
main = getArgs
   >>= processArgs

processArgs :: [String] -> IO ()
processArgs      [] = print test1
processArgs     [_] = print "Only one list provided."
processArgs (x:y:_) =
  case readLists x y of
    Nothing        -> print "Could not read the arguments"
    Just (lhs,rhs) -> print $ simpleTest lhs rhs

readLists :: String -> String -> Maybe (SamplePacked, SamplePacked)
readLists x y = do
  lhs <- readMay x :: Maybe [Word16]
  rhs <- readMay y :: Maybe [Word16]
  pure (fromList lhs, fromList rhs)
