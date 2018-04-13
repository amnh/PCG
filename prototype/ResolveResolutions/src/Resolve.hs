-----------------------------------------------------------------------------
-- |
-- Module      :  Resolve
-- Copyright   :  (c) 2018 Eric Ford
-- License     :  BSD-style
--
-- Maintainer  :  eford@gradcenter.cuny.edu
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Resolve where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..), fromList, (!!))
import Data.Maybe
import Data.Semigroup
import Data.Void
import File.Format.Newick
import Prelude             hiding ((!!))
import Text.Megaparsec

import System.Environment  -- for command-line arguments.
import System.Exit


main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        []            -> putStrLn "No arguments supplied. Need a Newick file name."
        inputfileName : _ -> do
            inputStream  <- readFile inputfileName
            case parse' newickStreamParser inputfileName inputStream of
                Left  error  -> putStrLn $ parseErrorPretty' inputfileName error
                Right forest -> print $ fmap resolveAllTrees <$> forest
    where
       parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
       parse' = parse


-- |
-- Takes in a a 'NewickNode', the root of a tree which may or may not be completely resolved, and returns a
-- 'NewickForest', the list of all possible resolutions of the tree rooted at the input 'NewickNode'.
resolveAllTrees :: NewickNode -> NewickForest
resolveAllTrees root =
    case descendants root of
        []   -> root :| []
        x:xs -> fromList . foldl makeNode [] . generateSubsets (x:xs) . length $ x:xs
    where
        makeNode acc (lhs, rhs) =
            fromJust (newickNode (foldMap toList $ toList ((resolveAllTrees <$> lhs) <> (resolveAllTrees <$> rhs))) Nothing Nothing) : acc


-- |
-- Takes in a 'NewickForest', i.e. a nonempty list of 'NewickNode's, and returns a list of 2-tuples of 'NewickForest's,
-- where each 2-tuple is a pair of 'NewickForest's that are subsets of the input forest. The union of those
-- subsets is the input forest, they do not intersect, and neither can be the empty set.
-- Throws an error if |input forest| < 3.
generateSubsets :: [NewickNode] -> Int -> [(NewickForest, NewickForest)]
generateSubsets inNodes numTrees =
    case inNodes of
        x:[]    -> error "Exactly one element found in a set. Need three."
        x:[y]   -> error "Exactly two elements found in a set. Need three."
        x:[y,z] -> [ (x :| [], y :| [z])
                   , (y :| [], x :| [z])
                   , (z :| [], x :| [y])
                   ]
        xs      -> snd $ foldl getPieces (0, []) xs

    where
        getPieces :: (Int, a) -> [NewickNode] -> (Int, [(NewickForest, NewickForest)])
        getPieces (index, acc) inputList
            -- Ignore any sets that have ordinality greater than half the size of the input set, as they'll already
            -- have been included during first half of fold.
            -- Likewise, stop folding when there are at most two elements in the last set, otherwise we end up
            -- skipping the base case, which requires |m| == 3.
            | numTrees - index - 1 > 2 && index <= numTrees / 2 =
                case inputList of
                    -- []    -> error "Something went wrong."
                    x:[] -> (index, x)
                    x:xs -> foldl f [] $ generateSubsets xs $ numTrees - index - 1 -- x has been taken care of above
            | otherwise = (succ index, acc)
        -- f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> Int -> [(NewickForest, NewickForest)]
        f tupleList (lhs, rhs) idx = (idx, (inNodes !! idx <> lhs, rhs) : tupleList)



-- Get line from file on argv
-- For each line:
--     Call Newick parser
-- For each tree:
--     Call resolveAllTrees
-- Output results
