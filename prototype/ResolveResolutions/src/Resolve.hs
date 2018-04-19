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
-- import Prelude             hiding ((!!))
import Text.Megaparsec

import System.Environment  -- for command-line arguments.
import System.Exit


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
        xs -> fromList . foldl makeNode [] . generateSubsets (toList xs) $ length xs
    where
        makeNode acc (lhs, rhs) =
            fromJust (newickNode (foldMap toList $ toList ((resolveAllTrees <$> lhs) <> (resolveAllTrees <$> rhs))) Nothing Nothing) : acc


-- |
-- Takes in a 'NewickForest', i.e. a nonempty list of 'NewickNode's, and returns a list of 2-tuples of 'NewickForest's,
-- where each 2-tuple is a pair of 'NewickForest's that are subsets of the input forest. The union of those
-- subsets is the input forest, they do not intersect, and neither can be the empty set.
-- numNodes is the number of nodes in inNodes.
-- Throws an error if |input forest| < 3.
generateSubsets :: [NewickNode] -> Int -> [(NewickForest, NewickForest)]
generateSubsets inNodes numNodes =
    case inNodes of
        x:[]    -> error "Exactly one element found in a set. Need three."
        x:[y]   -> error "Exactly two elements found in a set. Need three."
        x:[y,z] -> [ (x :| [], y :| [z])    -- This is recursion base case.
                   , (y :| [], x :| [z])
                   , (z :| [], x :| [y])
                   ]
        xs      -> snd $ foldl getPieces (0, []) xs
    where

        -- For each newick node in input produce a 2-tuple of an int and a tuple. The int is just the index of the current
        -- member being examined. The second tuple item is a list of tuples, where each 2-tuple is a pair of 'NewickForest's
        -- that are subsets of the input forest. The union of those subsets is the input forest, they do not intersect,
        -- and neither can be the empty set.
        getPieces :: (Int, [(NewickForest, NewickForest)]) -> [NewickNode] -> (Int, [(NewickForest, NewickForest)])
        getPieces (curIndex, forestTupleAcc) (curMember:rest) = (succ curIndex, firstMember <> secondMember)
            where
                -- For each single member, curMember, create tuple (curMember, inNodeList - curMember)
                -- where (inNodeList - curMember) is concatenation of members before curMember in list, i.e. forestTupleAcc
                -- and members after curMember, i.e. rest.
                -- curMember is head of inNodeList
                firstMember = [(curMember:|[], curMember:|[])] --TODO: I'm not creating inNodeList - curMember here

                -- Now append all remaining subsets recursively.
                -- Ignore any sets that have ordinality greater than half the size of the input set, as they'll already
                -- have been included during first half of fold.
                -- Likewise, stop folding when there are at most two elements in the last set, otherwise we end up
                -- skipping the base case, which requires |m| == 3.
                -- Using an index is not very functional, but does mean I don't have to keep doing O(n)
                -- length calculations.
                secondMember
                    | numNodes - curIndex - 1 > 2 && curIndex <= numNodes / 2 =
                        foldl f [] $ snd $ generateSubsets rest $ numNodes - curIndex - 1  -- x has been taken care of above
                    | otherwise = (curIndex, forestTupleAcc) -- Base case.
                f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> Int -> [(NewickForest, NewickForest)]
                f tupleList (lhs, rhs) idx = ((inNodes !! idx :| []) <> lhs, rhs) : tupleList



{-    case inNodeList of
                []             -> error "Empty list."                  -- Shouldn't hapapen because short-circuits in generateSubsets
                curMember:[]   -> (curIndex, curMember:forestTupleAcc) -- Base case: end of list.
                curMember:rest -> (succ curIndex, firstPiece <> secondPiece)
                    where
                        -- For each single member, curMember, create tuple (curMember, inNodeList - curMember)
                        -- where (inNodeList - curMember) is concatenation of members before curMember in list, i.e. forestTupleAcc
                        -- and members after curMember, i.e. rest.
                        -- curMember is head of inNodeList
                        firstPiece = [(curMember:|[], forestTupleAcc <> rest)]

                        -- Now append all remaining subsets recursively.
                        -- Ignore any sets that have ordinality greater than half the size of the input set, as they'll already
                        -- have been included during first half of fold.
                        -- Likewise, stop folding when there are at most two elements in the last set, otherwise we end up
                        -- skipping the base case, which requires |m| == 3.
                        -- Using an index is not very functional, but does mean I don't have to keep doing O(n)
                        -- length calculations.
                        secondPiece
                            | numNodes - curIndex - 1 > 2 && curIndex <= numNodes / 2 =
                                foldl f [] $ generateSubsets rest $ numNodes - curIndex - 1 -- x has been taken care of above
                            | otherwise = (curIndex, forestTupleAcc) -- Base case.
                        f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> Int -> (Int, [(NewickForest, NewickForest)])
                        f tupleList (lhs, rhs) idx = (idx, (inNodes !! idx <> lhs, rhs) : tupleList)
-}
