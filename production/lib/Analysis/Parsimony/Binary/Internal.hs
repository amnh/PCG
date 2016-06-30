-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions used accross optimization modules
--
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts #-}

module Analysis.Parsimony.Binary.Internal where

import Analysis.Parsimony.Binary.Constraints
import Analysis.Parsimony.Binary.DirectOptimization
import Analysis.Parsimony.Binary.Fitch
import Bio.Character.Dynamic.Coded
import Bio.Metadata
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node ()
import Bio.PhyloGraph.Node.Encoded
import Bio.PhyloGraph.Node.Final
import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Solution
--import Bio.PhyloGraph.Tree.Binary
--import Bio.PhyloGraph.Tree.Rose
--import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
--import Data.Maybe
import Data.Foldable
import Data.Function.Memoize (Memoizable)
import Data.IntMap           (IntMap)
import qualified Data.IntMap as IM
import Data.Key       hiding ((!))
import Data.Monoid
import Data.MonoTraversable
import Data.Ord              (comparing)
import Data.Vector           (Vector, (!), ifoldr)
import Prelude        hiding (lookup)

import Debug.Trace (trace)

-- !!!TODO: Remove weighting.

-- | Top level binary optimization wrapper to optimize over a solution
-- Takes in an overall weight and a solution
-- Returns a solution with any relevant values assigned (root cost, node assignments, etc. depending on optimization types).
-- Calls 'graphOptimization'.
solutionOptimization :: (SolutionConstraint' r f t n s m, Show (Element s)) => Double -> r -> r
solutionOptimization weighting inSolution = outForests 
    where
        meta = getMetadata inSolution
        outForests = setForests inSolution $ fmap (graphOptimization weighting meta) (getForests inSolution)

-- | Mapping function to optimize over a forest
-- Takes in an overall weight, a vector of metadata, and a forest
-- Returns a forest with relevant values assigned.
-- Calls 'allOptimization'.
graphOptimization :: (ForestConstraint' f t n s, Metadata m s, Show (Element s)) => Double -> Vector m -> f -> f
graphOptimization weighting meta inGraph = setTrees inGraph $ fmap (allOptimization weighting meta) (trees inGraph)

-- | Unified function to perform both the postorder and preorder passes (as relevant)
-- Takes in an overall weight, a vector of metadata, and a tree.
-- Returns a tree with values assigned.
-- This actually calls the postorder and preorder optimizations.
allOptimization :: (TreeConstraint' t n s, Metadata m s, Show (Element s)) => Double -> Vector m -> t -> t
allOptimization weighting meta inTree = secondPass
    where
        firstPass  = treeOptimizePostorder weighting inTree meta
        secondPass = treeOptimizePreorder firstPass meta

-- | Optimization postorder wrapper to perform relevant algorithm at all nodes
-- Takes in an overall weight, a tree, and a vector of metadata
-- Returns a tree with values assigned
-- Correctly handles roots, leaves, and nodes with only one child
treeOptimizePostorder :: (TreeConstraint' t n s, Metadata m s, Show (Element s)) => Double -> t -> Vector m -> t
treeOptimizePostorder weighting tree meta = tree `update` (rootNode : nonRootNodes)
  where
    -- We recursively decorate all nodes in the tree, then return the updated tree
    (rootNode, nonRootNodes) = treeInternalPostorderTraversal weighting (root tree) tree meta

-- | Internal postorder optimization pass
-- takes in a weight, a current node, a tree, and a vector of metadata
-- returns a list of nodes that have had changes applied to them
-- By using this node accumulation scheme, we save some complexity over simply always dealing with a tree
treeInternalPostorderTraversal :: (TreeConstraint' t n s, Metadata m s, Show (Element s)) => Double -> n -> t -> Vector m -> (n, [n])
treeInternalPostorderTraversal weighting node tree meta = (decoratedSelf, decoratedSubtree)
  where
      -- After applying the traversal logic to the subtree of this node
      -- we count the mutated children of this node to determine how to decorate this node.
      --
      -- If the node has two (or more) children, we apply direct optimization and decorate this node with
      -- the "gapped" and "ungapped" results of the direct optimization coparison of the two children.
      --
      -- If the node has a single child, we decorate this node with the exact values of the child node.
      --
      -- If the node has no children, we decorate the leaf node with cost values of zer.
      decoratedSelf =
        case children' of
          left:right:_ -> nodeOptimizePostorder weighting node left right meta
          child:_      -> decorateInternal child node
          _            -> decorateLeaf node

      -- We apply the traversal logic to the nodes children, returning back [(n,[n])]
      -- where the first componet ofeach tuple is a mutated child of this node
      -- and where the second component of the tuple is the list of mutated nodes in the mutated child's subtree.
      recursiveResult            = (\x -> treeInternalPostorderTraversal weighting x tree meta) <$> children node tree

      -- We extract the /only/ mutated children nodes from the resursive result.
      children'                  = fst <$> recursiveResult

      -- We combine the mutated subtrees with the mutated children to create a new mutated subtree for this node.
      decoratedSubtree           = children' <> concatMap snd recursiveResult

      -- Logic for decorating a leaf node
      decorateLeaf     leafNode  = setTotalCost 0.0 $ setLocalCost 0.0 leafNode

      -- Logic for decorating a node with exactly one child.
      -- These nodes are malformed in most (all?) tree topologies.
      decorateInternal otherNode = -- setTemporary   (getTemporary        otherNode)
                                   setPreliminaryGapped   (getPreliminaryGapped   otherNode)
                                 . setPreliminaryUngapped (getPreliminaryUngapped otherNode)
                                 . setLeftAlignment       (getLeftAlignment       otherNode)
                                 . setRightAlignment      (getRightAlignment      otherNode)
                                 . setTotalCost           (getTotalCost           otherNode)
                                 . setLocalCost           (getLocalCost           otherNode)

-- | Wrapper function to perform optimization on a node during the postorder pass
-- Essentially map decision function that selects and performs the correct optimization over the sequence of characters.
-- Takes in an overall weight, a current node, the left child, the right child, and a vector of metadata
-- Outputs a node with the correct sequences and costs assigned.
nodeOptimizePostorder :: (NodeConstraint' n s, Metadata m s, Show (Element s)) => Double -> n -> n -> n -> Vector m -> n
nodeOptimizePostorder weighting curNode lNode rNode meta = summedTotalCost `setTotalCost` res
    where
        summedTotalCost = sum [getLocalCost res, getTotalCost lNode, getTotalCost rNode]
        res             = ifoldr chooseOptimization curNode meta

        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization curPos metadataStructure setNode
            -- TODO: Compiler error maybe below with comment structures and 'lets'
            | getIgnored metadataStructure = setNode
            | getType metadataStructure == Fitch =
                let (assign, _, local) = preorderFitchBit curWeight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) metadataStructure
                in -- addTemporary temp
                   addLocalCost (local * curWeight * weighting)
                 . addAlign assign
                 . addPreliminary assign
                 $ setNode
            | getType metadataStructure == DirectOptimization =
                let (ungapped, cost, gapped, leftAlignment, rightAlignment) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) $ getCosts metadataStructure
                in addLocalCost (cost * curWeight * weighting)
                 . addAlign          gapped
                 . addPreliminary    ungapped
                 . addLeftAlignment  leftAlignment
                 . addRightAlignment rightAlignment
                 $ setNode
            | otherwise = error "Unrecognized optimization type"
            where curWeight = getWeight metadataStructure

        addPreliminary    = addToField setPreliminaryUngapped getPreliminaryUngapped
        addLeftAlignment  = addToField setLeftAlignment  getLeftAlignment
        addRightAlignment = addToField setRightAlignment getRightAlignment
        addAlign          addVal node = addToField setPreliminaryGapped   getPreliminaryGapped   addVal node
--        addTemporary   addVal node = addToField setTemporary   getTemporary        addVal node
--        addTotalCost   addVal node = setTotalCost (addVal + getTotalCost node) node
        addLocalCost      addVal node = setLocalCost (addVal + getLocalCost node) node

-- | Wrapper for the preorder
-- Takes in a tree and a vector of metadata,
-- returns a tree with relevant nodes assigned.
-- This wrapper allows us to deal correctly with root passing to preorder algorithms
treeOptimizePreorder :: (TreeConstraint' t n s, Metadata m s, Show (Element s)) => t -> Vector m -> t
treeOptimizePreorder tree meta = tree `update` treeInternalPreorderTraversal Nothing (root tree) tree meta

-- | Internal preorder pass that does the main recursion
-- Takes in a current node, the tree, and a vector of metadata;
-- returns a list of nodes that have been updated.
-- As in the postorder, this method saves on some time complexity.
treeInternalPreorderTraversal :: (TreeConstraint' t n s, Metadata m s, Show (Element s)) => Maybe n -> n -> t -> Vector m -> [n]
treeInternalPreorderTraversal parentNode node tree meta  = 
  case children' of
      left:right:_ -> let mutatedSelf = nodeOptimizePreorder node left right parentNode meta 
                      in  mutatedSelf : concatMap (\x -> treeInternalPreorderTraversal (Just mutatedSelf) x tree meta) children'
      _            -> concatMap (\x -> treeInternalPreorderTraversal (Just node) x tree meta) children'
  where
      children' = children node tree

-- | Wrapper function to perform optimization on a node during the preorder pass.
-- As in the postorder, it selects an optimization for each character, then groups the optimized characters together and assigns them to the node.
-- Takes in a current node, left child, right child, parent node, and vector of metadata,
-- returns a node with everything assigned.
nodeOptimizePreorder :: (NodeConstraint' n s, Metadata m s, Show (Element s)) => n -> n -> n -> Maybe n -> Vector m -> n
nodeOptimizePreorder curNode lNode rNode pNode = ifoldr chooseOptimization curNode
    where
        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization i metadataStructure setNode
            | getType metadataStructure == Fitch =
              case pNode of
                Nothing -> addToField setFinal getFinal (constructDynamic []) setNode -- TODO: This is broken, please don't ever use the code until after we have proper Static/Dynamic sequence partioning
                Just parentNode -> 
                  let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign parentNode ! i) {- (getTemporary curNode ! i) -} (constructDynamic []) metadataStructure
                  in addToField setFinal getFinal finalAssign setNode
            | getType metadataStructure == DirectOptimization =  --TODO: do we grab the gapped or not?
              case pNode of
                Nothing -> addToField setFinal       getFinal       (getPreliminaryUngapped setNode ! i)
                         . addToField setFinalGapped getFinalGapped (getPreliminaryGapped   setNode ! i)
                         $ setNode
                Just parentNode -> 
                  let costStructure    = getCosts metadataStructure
                      childCharacter   =  (\x -> trace ("childCharacter: "  <> show x) x) $ getChildCharacterForDoPreorder curNode ! i
                      parentCharacter  =  (\x -> trace ("parentCharacter: " <> show x) x) $ getFinal parentNode ! i
                      (_, _, derivedAlignment, _, childAlignment) = naiveDO parentCharacter childCharacter costStructure
                      newGapIndicies   =  (\x -> trace ("newGapIndices: "   <> show x) x) $  newGapLocations childCharacter $ (\x -> trace ("childAlignment: "   <> show x) x) $ childAlignment
                      leftCharacter    =  (\x -> trace ("leftCharacter: "   <> show x) x) $  insertNewGaps newGapIndicies $ getLeftAlignment  curNode ! i
                      rightCharacter   =  (\x -> trace ("rightCharacter: "  <> show x) x) $  insertNewGaps newGapIndicies $ getRightAlignment curNode ! i
                      (_, finalUngapped, finalGapped) = threeWayMean costStructure derivedAlignment leftCharacter rightCharacter
                  in  addToField setFinal       getFinal       finalUngapped
                    . addToField setFinalGapped getFinalGapped finalGapped
                    $ setNode
            | otherwise = error "Unrecognized optimization type"

-- | getForAlign returns the sequences from a node, where the node type is either 'EncodedNode' or 'PreliminaryNode'.
-- preliminary alignment 
getForAlign :: (PreliminaryNode n s, EncodedNode n s, SeqConstraint' s) => n -> Vector s
getForAlign node
    | null (getPreliminaryGapped node) && null (getPreliminaryUngapped node) = getEncoded node
    | null $ getPreliminaryGapped node                                       = getPreliminaryUngapped node
    | otherwise                                                              = getPreliminaryGapped node

-- | Retreives the correct sequence of dynamic characters for the direct
--   optimization preorder traversal from the child node. We conditionally
--   select one of two fields. The gapped preliminary node assignment is
--   preferenced and returned if not null. It is assumed that all internal nodes
--   will have a non null vector of preliminary node assignemnt characters. If
--   the gapped preliminary vector is null, itis assumed that the node is a leaf
--   node and the original dynamic character encodings are returned.
getChildCharacterForDoPreorder ::  (PreliminaryNode n s, EncodedNode n s) => n -> Vector s
getChildCharacterForDoPreorder node
  | not . null $ getPreliminaryUngapped node = getPreliminaryGapped node
  | otherwise                                = getEncoded node

-- | addToField takes in a setter fn, a getter fn, a value and a node.
-- It then gets the related value from the node, adds to it the passed value,
-- and sets that value on the node. It returns a new node with the newly computed value set.
addToField :: NodeConstraint' n s => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node

-- 
newGapLocations :: (EncodableDynamicCharacter c, Show c, Show (Element c)) => c -> c -> IntMap Int
--newGapLocations originalChar newChar | trace ("o: " <> show originalChar <> "n: " <> show newChar) False = undefined
newGapLocations originalChar newChar
  | olength originalChar == olength newChar = mempty
  | otherwise                               = newGaps
  where
    (_,_,newGaps) = ofoldl' f (otoList originalChar, 0, mempty) newChar
    gap = getGapChar $ newChar `indexChar` 0
--    f a e | trace (show a <> " " <> show e) False = undefined
    f (  [], i, is) e
      | e == gap  = ([], i, IM.insertWith (+) i 1 is)
      | otherwise = ([], i, is)
    f (x:xs, i, is) e
      | e == gap && x /= gap = (x:xs, i  , IM.insertWith (+) i 1 is)
      | otherwise            = (  xs, i+1, is)

insertNewGaps :: EncodableDynamicCharacter c => IntMap Int -> c -> c
insertNewGaps insertionIndicies = constructDynamic . foldMapWithKey f . otoList
  where
    f i e =
      case i `lookup` insertionIndicies of
        Nothing -> [e]
        Just n  -> replicate n (getGapChar e) <> [e]
      
threeWayMean :: (Show (Element c),Show c, EncodableDynamicCharacter c, Memoizable (Element c)) => CostStructure -> c -> c -> c -> (Double, c, c)
threeWayMean _ char1 char2 char3 | trace (mconcat [show char1, show char2, show char3]) False = undefined
threeWayMean costStructure char1 char2 char3
  | not uniformLength = error $ "Three sequences supplied to 'threeWayMean' function did not have uniform length." <> show char1 <> show char2 <> show char3
  | otherwise         = (sum costValues, constructDynamic $ filter (/= gap) meanStates, constructDynamic meanStates)
  where
    gap                 = getGapChar $ char1 `indexChar` 0
    uniformLength       = olength char1 == olength char2 && olength char2 == olength char3
    (meanStates, costValues) = unzip $ zipWith3 f (otoList char1) (otoList char2) (otoList char3)
    f a b c = minimumBy (comparing snd)
            $ (\x -> trace (show x) x)
            [ getOverlap a b costStructure
            , getOverlap a c costStructure
            , getOverlap b c costStructure
            ]
