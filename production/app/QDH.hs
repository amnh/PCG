module Main (main) where

import Analysis.ImpliedAlignment.DynamicProgramming
import Analysis.Parsimony.Binary.Optimization
import Control.Applicative ((<|>))
import Data.Maybe          (fromMaybe)
import Safe                (headMay, readMay, tailMay)
import System.Environment  (getArgs)
import Test.Custom

-- import Debug.Trace (trace)

main :: IO ()
main = getArgs
   >>= compute . handleInput

handleInput :: [String] -> SimpleTree
handleInput args = fromMaybe exampleTree $ trySimpleTree args <|> trySimpleBinaryTree args

compute :: SimpleTree -> IO ()
compute = print . deriveImpliedAlignments defMeta . allOptimization 1 defMeta 

trySimpleTree :: [String] -> Maybe SimpleTree
trySimpleTree xs = do
  rootIndex <- readMay =<< headMay xs :: Maybe Int
  alphabet  <- readMay =<< headMay =<< tailMay xs :: Maybe String
  mapping   <- readMay =<< headMay =<< tailMay =<< tailMay xs :: Maybe [(Int,String,[Int])]
  pure $ createSimpleTree rootIndex alphabet mapping

trySimpleBinaryTree :: [String] -> Maybe SimpleTree
trySimpleBinaryTree xs = do
  leafValues <- readMay =<< headMay xs :: Maybe [String]
  pure $ createBinary leafValues

exampleTree :: SimpleTree
exampleTree = createSimpleTree 8 "ACTG"
  [ (2,    "",[0,1])
  , (0, "ACG",   [])
  , (1,"ACGT",   [])
  , (3,"ACTA",   [])
  , (4,    "",[2,3])
  , (5, "ACT",   [])
  , (6,    "",[4,5])
  , (7, "ACA",   [])
  , (8,    "",[6,7])
  ]
{--}
{--
     $ createSimpleTree 0 "ACTG"
     [ (0,     "", [1,2])
     , (1,     "", [3,4])
     , (2,     "", [5,6])
     , (3,     "", [7,8])
     , (4,  "ACA",    [])
     , (5, "ACTA",    [])
     , (6, "ACGT",    [])
     , (7,  "ACG",    [])
     , (8,  "ACT",    [])
     ]
--}
