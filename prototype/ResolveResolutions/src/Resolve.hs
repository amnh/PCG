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
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe
import Data.Semigroup
import Data.Void
import File.Format.Newick
import Text.Megaparsec
import System.Environment  -- for command-line arguments.


-- |
-- Get line from file on argv
-- For each line:
--     Call Newick parser
-- For each tree:
--     Call resolveAllTrees
-- Output results
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        []            -> putStrLn "No arguments supplied. Need a Newick file name."
        inputfileName : _ -> do
            inputStream  <- readFile inputfileName
            case parse' newickStreamParser inputfileName inputStream of
                Left  errMsg -> putStrLn $ parseErrorPretty' inputfileName errMsg
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
        xs -> fromList . foldl makeNode [] $ generateSubsets [] (toList xs) inputLength inputLength
            where
                makeNode acc (lhs, rhs) =
                    fromJust (newickNode (foldMap toList $ toList ((resolveAllTrees <$> lhs) <> (resolveAllTrees <$> rhs))) Nothing Nothing) : acc
                inputLength = length xs


-- |
-- Takes in a list of 'NewickNode's, and returns a list of 2-tuples of 'NewickForest's,
-- where each 2-tuple is a pair of 'NewickForest's that are subsets of the input list. The union of those
-- subsets is the input forest, they do not intersect, and neither can be the empty set.
-- numNodes is the number of nodes in inNodes.
-- Throws an error if |input forest| < 3.
generateSubsets :: [NewickNode] -> [NewickNode] -> Int -> Int -> [(NewickForest, NewickForest)]
generateSubsets previous inNodes originalLength curLength =
    case inNodes of
        []      -> [] -- error "Empty list not allowed. Minimum length is three."
        [_]     -> [] -- error "Exactly one element found in a list. Need at least three."
        [x,y]   -> [ (x:|[], y:|[]) ] -- error "Exactly two elements found in a list. Need at least three."
        x:[y,z] -> [ (x :| [], y :| [z])    -- This is recursion base case.
                   , (y :| [], x :| [z])
                   , (z :| [], x :| [y])
                   ]
        x:xs      -> fst . foldl getPieces ([], xs) $ x:xs -- Need to accumulate
                                                           -- the list of tuples
                                                           -- the current index
                                                           -- the previous members
                                                           -- the following members
    where
        -- For each newick node in input produce a 2-tuple of an int and a tuple. The int is just the index of
        --  the current member being examined. The second tuple item is a list of tuples, where each 2-tuple
        -- is a pair of 'NewickForest's that are subsets of the input forest. The union of those subsets is
        -- the input forest, they do not intersect, and neither can be the empty set.
        getPieces :: ([(NewickForest, NewickForest)], [NewickNode]) -> NewickNode -> ([(NewickForest, NewickForest)], [NewickNode])
        getPieces (forestTupleAcc, following) curMember = (firstSet <> secondSet <> forestTupleAcc, following)
            where
                -- For each single member, curMember, create tuple (curMember, inNodeList - curMember)
                -- where (inNodeList - curMember) is concatenation of members before curMember in list, i.e. forestTupleAcc
                -- and members after curMember, i.e. rest.
                -- curMember is head of inNodeList
                firstSet :: [(NewickForest, NewickForest)]
                firstSet = [(fromList [curMember], fromList $ previous <> following)]

                -- Now append all remaining subsets recursively.
                -- Ignore any sets that have ordinality greater than half the size of the input set, as they'll already
                -- have been included during first half of fold.
                -- Likewise, stop folding when there are at most two elements in the last set, otherwise we end up
                -- skipping the base case, which requires |m| == 3.
                -- Using an index is not very functional, but does mean I don't have to keep doing O(n)
                -- length calculations.
                secondSet :: [(NewickForest, NewickForest)]
                secondSet =
                    if curLength <= quot originalLength 2  -- && (curLength - curLength - 1 > 2)
                    then
                        foldl f [] $ generateSubsets (curMember:previous) following originalLength (curLength - 1) -- curMember has been taken care of above
                    else forestTupleAcc -- Base case.
                f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> [(NewickForest, NewickForest)]
                f tupleList (lhs, rhs) = ((fromList [inNodes !! (originalLength - curLength)]) <> lhs, rhs) : tupleList
