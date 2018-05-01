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
                Right forest -> mapM_ putStrLn . sconcat $ fmap (renderNewickForest . resolveTree) <$> forest
    where
       parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
       parse' = parse

{-
main function

resolve_tree :: node -> forest
  child_sets <- generate_subset_pairs
                      descendents node
  for each child set compile list of pairings
  for each pairing list
  left_subtrees <- map resolve_tree over
                      map left child_sets
  right_subtrees <- map resolve_tree over
                      map right child_sets
  output <- map create_node over
             [(i.j) | i <- left, j <- right]


generate_subsets_pairs :: forest -> [forest]

get_subtrees :: node -> forest
create node

generate subtree
-}

-- |
-- Takes in a a 'NewickNode', the root of a tree which may or may not be completely resolved, and returns a
-- 'NewickForest', the list of all possible resolutions of the tree rooted at the input 'NewickNode'.
resolveTree :: NewickNode -> NewickForest
resolveTree root = -- trace ("root " <> show root) $
    case descendants root of
        [] -> root :| []
        [x] -> resolveTree x
        xs -> allSubtrees {- pure $ foldl makeNode [] subtrees -}
            where
                -- childTuples is a list of partitions as tuples.
                -- Each partition is the nodes in one set of subtrees for the input node.
                childTuples = generateSubsetPairs [] (toList xs) inputLength inputLength
                inputLength = length xs

                -- allSubtrees is a 'NonEmpty' list of binary 'NewickNode's, where each node represents one possible
                -- resolution of the two subtrees, and the whole list represents all possible resolutions.
                -- allSubtrees :: NewickForest
                -- allSubtrees = trace ("leftSubtrees "    <> show leftSubtrees <>
                --                      "\nrightSubtrees " <> show rightSubtrees) $
                --     fromList [ makeNode $ toList (i <> j) | i <- leftSubtrees, j <- rightSubtrees]
                allSubtrees = fromList $ foldMap f childTuples

                -- combineSubtrees (leftChild, rightChild) = (resolveTree leftSubtree) <> (resolveTree rightSubtree)
                --     where
                --         leftSubtree  = makeNode leftChildren
                --         rightSubtree = makeNode rightChildren

                -- left & right subtrees are respectively all possible resolutions of the left & right leaf partitions
                -- returned by generateSubsetPairs and held in childTuples. Each is therefore a list of 'NewickForest's
                -- as each forest is a non-empty list of resolutions.
                -- leftSubtrees :: [NewickForest]
                -- (leftSubtrees, rightSubtrees) = trace ("childTuples " <> show childTuples) $
                --                 map f childTuples
                -- rightSubtrees = map (resolveTree . makeNode . toList . snd) childTuples
                -- subtrees      = [(i.j) | i <- leftSubtrees, j <- rightSubtrees ]
                f (left, right) =
                    [ makeNode $ i : [j] | i <- toList leftSubtrees
                                         , j <- toList rightSubtrees
                    ]
                    where
                        (leftSubtrees, rightSubtrees) = (resolveTree . makeNode $ toList left, resolveTree . makeNode $ toList right)
                -- newRoot is a forest of possible subtrees
                -- newRoots       = makeNode $ (foldMap fst allSubtrees) <> (foldMap snd allSubtrees)

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
                -- combinedSet = [i <> j | i <- resolveTree firstSet,
                --                         j <- secondSet ]
                -- For each single member, curMember, create tuple (curMember, inNodeList - curMember)
                -- where (inNodeList - curMember) is concatenation of members before curMember in list, i.e. forestTupleAcc
                -- and members after curMember, i.e. following.
                -- curMember is head of inNodeList
                firstSet :: [(NewickForest, NewickForest)]
                firstSet = [(pure curMember, fromList $ previous <> following)]
                -- firstSet = [(pure curMember, resolveTree <$> (previous <> following))]
                -- firstSet = [(pure i, j) | i <- [curMember]
                --                         , j <- toList $ resolveTree <$> (previous <> following)
                --            ]

                -- Now append all remaining subsets recursively.
                -- Ignore any sets that have ordinality greater than half the size of the input set, as they'll already
                -- have been included during first half of fold.
                -- Likewise, stop folding when there are at most two elements in the last set, otherwise we end up
                -- skipping the base case, which requires |m| == 3.
                -- Using an index is not very functional, but does mean I don't have to keep doing O(n)
                -- length calculations.
                secondSet :: [(NewickForest, NewickForest)]
                secondSet =
                    if (originalLength - curLength - 1 > 2) -- && curLength <= quot originalLength 2
                    then
                        -- [(i, j) | i <- resolveTree <$> curMember:previous
                        --         , j <- resolveTree <$> following
                        -- ]
                        foldl f [] $ generateSubsetPairs (curMember:previous) following originalLength (curLength - 1) -- curMember has been taken care of above
                    else forestTupleAcc -- Base case.
                f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> [(NewickForest, NewickForest)]
                f tupleList (lhs, rhs) = ((pure (inNodes !! (originalLength - curLength))) <> lhs, rhs) : tupleList

{-

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
                firstSet = [(pure curMember, fromList $ previous <> following)]

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
                        foldl f [] $ generateSubsetPairs (curMember:previous) following originalLength (curLength - 1) -- curMember has been taken care of above
                    else forestTupleAcc -- Base case.
                f :: [(NewickForest, NewickForest)] -> (NewickForest, NewickForest) -> [(NewickForest, NewickForest)]
                f tupleList (lhs, rhs) = ((pure (inNodes !! (originalLength - curLength))) <> lhs, rhs) : tupleList

-}

{-                makeNode acc (lhs, rhs) =
                    -- Following notes are for Eric's educational purposes.
                    fromJust (  -- Unsafely unwrap the Maybe type.
                      newickNode  -- Apply the smart constructor
                        ( -- First argument to the smart constructor between parens
                        foldMap  -- Fold over the structure by applying the transformation (a -> m) to each element
                                 -- then "Concatenate" all the transformed elements using (<>).
                                 -- This will flatten a "list of lists" to a list.
                          toList -- The function (a -> m) to be applied to each element of the structure.
                            (
                              (resolveTree <$> lhs) -- Apply the function recursively to the left hand side
                              <>  -- "Concatenate" the recursive results (list concatenation).
                              (resolveTree <$> rhs) -- Apply the function recursively to the right hand side
                            )
                        )
                        Nothing -- Second argument to newickNode
                        Nothing -- Third argument to newickNode
                      ) : acc -- cons result of newickNode call to front of accumulator
-}
{-
        xs -> fromList . foldl makeNode [] $ generateSubsetPairs [] (toList xs) inputLength inputLength
            where
                makeNode acc (lhs, rhs) =
                    -- Following notes are for Eric's educational purposes.
                    fromJust (  -- Unsafely unwrap the Maybe type.
                      newickNode  -- Apply the smart constructor
                        ( -- First argument to the smart constructor between parens
                        foldMap  -- Fold over the structure by applying the transformation (a -> m) to each element
                                 -- then "Concatenate" all the transformed elements using (<>).
                                 -- This will flatten a "list of lists" to a list.
                          toList -- The function (a -> m) to be applied to each element of the structure.
                            (
                              (resolveTree <$> lhs) -- Apply the function recursively to the left hand side
                              <>  -- "Concatenate" the recursive results (list concatenation).
                              (resolveTree <$> rhs) -- Apply the function recursively to the right hand side
                            )
                        )
                        Nothing -- Second argument to newickNode
                        Nothing -- Third argument to newickNode
                        ) : acc -- cons result of newickNode call to front of accumulator
                inputLength = length xs
-}
