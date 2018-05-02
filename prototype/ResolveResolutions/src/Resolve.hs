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
import Debug.Trace
import System.Environment  -- for command-line arguments.


-- |
-- Get line from file on argv
-- For each line:
--     Call Newick parser
-- For each tree:
--     Call resolveTree
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
                -- Right forest -> print $ fmap resolveTree <$> forest
                -- Right forest -> mapM_ putStrLn . sconcat $ fmap (renderNewickForest . resolveTree) <$> forest
                Right forest -> mapM_ putStrLn . sconcat $ fmap (unlines . toList . fmap renderNewickString . resolveTree) <$> forest
    where
       parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
       parse' = parse


-- |
-- Take a 'NewickNode' and map over its descendents to render the entire string in Newick format.
renderNewickString :: NewickNode -> String
renderNewickString inNode =
    case descendants inNode of
        []    -> fromJust $ newickLabel inNode
        [x]   -> mconcat [ "(", renderNewickString x, ")" ]
        x:y:_ -> mconcat [ "(", renderNewickString x, ", ", renderNewickString y, ")" ]


-- |
-- Takes in a a 'NewickNode', the root of a tree which may or may not be completely resolved, and returns a
-- 'NewickForest', the list of all possible resolutions of the tree rooted at the input 'NewickNode'.
resolveTree :: NewickNode -> NewickForest
resolveTree root = -- trace ("root " <> show root) $
    case descendants root of
        [] -> root :| []
        [x] -> resolveTree x -- this was non-obvious. Subtrees are Nodes themselves, so must be unpacked.
        xs -> allSubtrees {- pure $ foldl makeNode [] subtrees -}
            where
                -- childTuples is a list of partitions as tuples.
                -- Each partition is the nodes in one set of subtrees for the input node.
                childTuples = generateSubsetPairs [] (toList xs) inputLength inputLength
                inputLength = length xs

                -- allSubtrees is a 'NonEmpty' list of binary 'NewickNode's, where each node represents one possible
                -- resolution of the two subtrees, and the whole list represents all possible resolutions.
                allSubtrees = fromList $ foldMap f childTuples

                -- left & right subtrees are respectively all possible resolutions of the left & right leaf partitions
                -- returned by generateSubsetPairs and held in childTuples. Each is therefore a list of 'NewickForest's
                -- as each forest is a non-empty list of resolutions.
                f (left, right) =
                    [ makeNode $ i : [j] | i <- toList leftSubtrees
                                         , j <- toList rightSubtrees
                    ]
                    where
                        -- Fold over both subtrees together, so you don't end up with extra resolutions on them,
                        -- which causes extra resolved trees, each of which has duplicated leaves or subtrees.
                        (leftSubtrees, rightSubtrees) = ( resolveTree . makeNode $ toList left
                                                        , resolveTree . makeNode $ toList right
                                                        )

                makeNode :: [NewickNode] -> NewickNode
                makeNode children = fromJust $ newickNode children Nothing Nothing


-- |
-- Takes in a list of 'NewickNode's, and returns a list of 2-tuples of 'NewickForest's,
-- where each 2-tuple is a pair of 'NewickForest's that are subsets of the input list. The union of those
-- subsets is the input forest, they do not intersect, and neither can be the empty set.
-- numNodes is the number of nodes in inNodes.
-- Throws an error if |input forest| < 3.
generateSubsetPairs :: [NewickNode] -> [NewickNode] -> Int -> Int -> [(NewickForest, NewickForest)]
generateSubsetPairs previous inNodes originalLength curLength = -- trace ("inNodes " <> show inNodes) $
    case inNodes of
        []      -> [] -- error "Empty list not allowed. Minimum length is three."
        [x]     -> [] -- error "Exactly one element found in a list. Need at least three."
        [x,y]   -> [ (pure x, pure y) ] -- error "Exactly two elements found in a list. Need at least three."
        x:[y,z] -> [ (pure x, y :| [z])    -- This is recursion base case.
                   , (pure y, x :| [z])
                   , (pure z, x :| [y])
                   ]
        x:xs      -> fst . foldl getPieces ([], xs) $ x:xs -- Need to accumulate
                                                           -- the list of tuples
                                                           -- the current index
                                                           -- the previous members
                                                           -- the following members
    where
        -- For each newick node in input produce a 2-tuple of an int and a tuple. The int is just the index of
        -- the current member being examined. The second tuple item is a list of tuples, where each 2-tuple
        -- is a pair of 'NewickForest's that are subsets of the input forest. The union of those subsets is
        -- the input forest, they do not intersect, and neither can be the empty set.
        getPieces :: ([(NewickForest, NewickForest)], [NewickNode]) -> NewickNode -> ([(NewickForest, NewickForest)], [NewickNode])
        getPieces (forestTupleAcc, following) curMember = (firstSet <> secondSet <> forestTupleAcc, following)
            where
                -- For each single member, curMember, create tuple (curMember, inNodeList - curMember)
                -- where (inNodeList - curMember) is concatenation of members before curMember in list, i.e. forestTupleAcc
                -- and members after curMember, i.e. following.
                -- curMember is head of inNodeList
                firstSet :: [(NewickForest, NewickForest)]
                firstSet = [(pure curMember, fromList $ previous <> following)]

                -- Now append all remaining subsets recursively.
                -- Stop folding when there are at most two elements in the last set, otherwise we end up
                -- skipping the base case, which requires |m| == 3.
                -- Using an index is not very functional, but does mean I don't have to keep doing O(n)
                -- length calculations.
                secondSet :: [(NewickForest, NewickForest)]
                secondSet =
                    if (originalLength - curLength - 1 > 2)
                    then
                        foldl f [] $ generateSubsetPairs (curMember:previous) following originalLength (curLength - 1) -- curMember has been taken care of above
                    else forestTupleAcc -- Base case.
                f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> [(NewickForest, NewickForest)]
                f tupleList (lhs, rhs) = ((pure (inNodes !! (originalLength - curLength))) <> lhs, rhs) : tupleList
