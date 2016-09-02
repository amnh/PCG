-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.DynamicProgramming
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}

-- TODO: Make an ImpliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment.DynamicProgramming where

import           Analysis.ImpliedAlignment.AlignmentContext
import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Analysis.ImpliedAlignment.Internal
import           Analysis.Parsimony.Binary.DirectOptimization
import           Bio.Metadata
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node     hiding  (Node,children, name)
import           Bio.PhyloGraph.Solution
import           Bio.Character.Dynamic.Coded
import           Control.Arrow                   ((&&&))
import           Data.Foldable
import qualified Data.HashMap.Lazy       as HM
import qualified Data.IntMap             as IM
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet             as IS
import           Data.Key
import           Data.List                       (sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Ord                        (comparing)
import qualified Data.Sequence           as Seq
import           Data.Vector                     (Vector)
import qualified Data.Vector             as V
import           Data.Vector.Instances           ()
import           Prelude                 hiding  (lookup,zip,zipWith)
import           Safe                            (tailMay)

data IndelEvents c e
   = IndelEvents
   { edgeInsertionEvents :: InsertionEvents c e
   , edgeDeletionEvents  :: DeletionEvents
   } deriving (Show)

data TreeReferences n
   = TreeRefs
   { rootRef    :: Int
   , nodeRefs   :: Vector n
   , parentRefs :: Vector Int
   , childRefs  :: Vector IntSet
   }

-- | Top level wrapper to do an IA over an entire solution
-- takes a solution
-- returns an AlignmentSolution
iaSolution :: (Eq n, SolutionConstraint r m f t n e s, IANode' n s, Show (Element s)) => r -> r
iaSolution inSolution = inSolution `setForests` fmap (`iaForest` getMetadata inSolution) (getForests inSolution)

-- | Simple wrapper to do an IA over a forest
-- takes in a forest and some metadata
-- returns an alignment forest
iaForest :: (Eq n, FoldableWithKey k, ForestConstraint f t n e s, IANode' n s, Metadata m s, Key k ~ Int, Show (Element s)) => f -> k m -> f
iaForest inForest inMeta = inForest `setTrees` fmap (deriveImpliedAlignments inMeta) (trees inForest)

-- TODO: make sure a sequence always ends up in FinalGapped to avoid this decision tree
-- | Simple function to get a sequence for alignment purposes
getForAlign :: NodeConstraint n s => n -> Vector s
getForAlign n 
--    | not . null $ getFinalGapped         n = getFinalGapped n
--    | not . null $ getPreliminaryUngapped n = getPreliminaryUngapped n 
    | not . null $ getSingle              n = getSingle n 
    | not . null $ getEncoded     n         = getEncoded n 
    | otherwise = mempty {-error "No sequence at node for IA to numerate"-}

-- | Decorates a tree with implied alignments of the leaf nodes given a tree
--   decorated with direct optimization annotations, along with suporting metadata.
deriveImpliedAlignments :: (Eq n, FoldableWithKey f, TreeConstraint t n e s, IANode' n s, Metadata m s, Key f ~ Int, Show (Element s))
                        => f m -> t -> t 
deriveImpliedAlignments sequenceMetadatas tree = foldlWithKey' f tree sequenceMetadatas
  where
    f t k m
      | getType m /= DirectOptimization = t
      | otherwise                       = numeration k (getCosts m) t

-- | The core derivation of an implied alignment for a given character on a tree.
--
--   Requires:
--
--   * The index of the character for which the implied alignment should be
--   derived, from the sequence of characters.
--
--   * The 'CostStructure' for that character
--
--   * The tree on which implies the alignment
-- 
numeration :: (Eq n, TreeConstraint t n e s, IANode' n s, Show (Element s)) => Int -> CostStructure -> t -> t
numeration sequenceIndex costStructure tree = tree `update` (snd <$> updatedLeafNodes)
  where
    -- | Precomputations used for reference in the memoization
    (TreeRefs rootIndex enumeratedNodes parentMapping childMapping) = precomputeTreeReferences tree
    rootNode        = root tree
    nodeCount       = length parentMapping

    -- | A memoized, constant access structure for the indel events of each edge in the tree.
    edgeIndels = HM.fromList $ (id &&& edgeValue) <$> edgeKeys
      where
        edgeKeys = [ (i,j) | i <- [0 .. nodeCount - 1], j <- [0 .. nodeCount - 1], j `oelem` (childMapping V.! i) ]
        edgeValue e@(i,j) = IndelEvents inserts deletes
          where
            parentCharacter = getSingle      (enumeratedNodes V.! i) V.! sequenceIndex
            childCharacter  = getForAlign    (enumeratedNodes V.! j) V.! sequenceIndex
            (deletes, !inserts, _, _) = comparativeIndelEvents e parentCharacter childCharacter costStructure

    -- | The root 'AlignmentContext'.
    --   Defines how 'InserionEvents' are accumulated in a post-order traversal
    --   to encapsulate the require global state information of the implied alignment.
    rootContext =
        Context
        { insertionEvents = totalInsertionEvents
        , psuedoCharacter = rootPsuedoCharacter
        }
      where
        rootPsuedoCharacter
          | not $ null leftOverInsertions = error "Leftover insertion events at root node. Out of range!"
          | otherwise = seqList
          where
            -- Maybe check for indicies after the length of the sequence.
            insertionMapping = IE.unwrap totalInsertionEvents
            characterLength  = olength $ getForAlign rootNode V.! sequenceIndex
            seqList          = (<> trailingInsertions) $ foldMap f [0..characterLength - 1]
            f k =
              case k `lookup` insertionMapping of
                Nothing -> [OriginalBase]
                Just n  -> replicate (length n) SoftGap <> [OriginalBase]
            trailingInsertions =
              case characterLength `lookup` insertionMapping of
                Nothing -> []
                Just n  -> replicate (length n) SoftGap
            leftOverInsertions = filter ((> characterLength) . fst) $ IM.assocs insertionMapping

        totalInsertionEvents = ofoldMap (allDescendantInsertions rootIndex) $ childMapping V.! rootIndex
            
        allDescendantInsertions i j = coalesce del ins . fmap (allDescendantInsertions j) . otoList $ childMapping V.! j
          where
            (IndelEvents ins del) = edgeIndels ! (i, j)

    -- | The 'AlignmentContext' for each node in the tree.
    --   Constructed using a recursive, memoized generating function
    --   with the root node as the base case.
    --   The generating function will implicitly perform a parralelizable pre-order traversal
    nodeContexts = V.generate (length enumeratedNodes) f
      where
        f nodeIndex
          | nodeIndex == rootIndex = rootContext
          | otherwise              = nodeContext
          where
            parentIndex       = parentMapping V.! nodeIndex
            parentContext     = nodeContexts  V.! parentIndex
            edgeKey           = (parentIndex, nodeIndex)
            IndelEvents inserts deletes = edgeIndels ! edgeKey
            nodeContext       = applyLocalEventsToAlignment edgeKey deletes inserts parentContext

    -- | We mutate the original nodes from the input tree to contain
    --    the implied alignment for each node.
    updatedNodes = foldMapWithKey f enumeratedNodes
      where
        f i n = [(i, deriveImpliedAlignment sequenceIndex (psuedoCharacter $ nodeContexts V.! i) n)]

    -- | We filter the 'updatedNodes' to only include the leaf nodes.
    --   We do this beacuse implied alignments on internal nodes doesn't make sense... or does it?
    updatedLeafNodes = filter ((`nodeIsLeaf` tree) . snd) updatedNodes


-- | Performs a preorder traversal to generate referential indexing structures
--   for refrencing actions to perform a tree.
precomputeTreeReferences :: TreeConstraint t n e s  => t -> TreeReferences n
precomputeTreeReferences tree =
     TreeRefs
     { rootRef    = 0
     , nodeRefs   = a
     , parentRefs = b
     , childRefs  = c
     }
  where
    (a,b,c) = V.unzip3 . V.fromList $ rmRefVal <$> sortBy (comparing refVal) tokens
    
    (_,tokens) = f (root tree) Nothing 0

    refVal   (i,_,_,_) = i
    rmRefVal (_,x,y,z) = (x,y,z)
                 
    f node parentRef counter = (counter', nodeResult : mconcat kids)
      where
        nodeResult = (counter, node, fromMaybe (-1) parentRef, IS.fromList $ refVal . head <$> kids)
        (counter',kids) = foldl' g (counter,[]) $ children node tree
          where
            g (subCounter,xs) n = (subCounter', ys:xs)
              where
                (subCounter',ys) = f n (Just counter) (subCounter+1)

-- | Calculates the 'IndelEvents' that occur given two sequences of an edge.
comparativeIndelEvents :: (Eq e, SeqConstraint s) => e -> s -> s -> CostStructure -> (DeletionEvents, InsertionEvents (Element s) e, s ,s)
comparativeIndelEvents edgeIdentifier ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
  | olength ancestorCharacter /= olength descendantCharacter = error errorMessage
  | otherwise                                                = -- (\x -> trace (show x) x) $
                                                               (DE resultingDeletionEvents, IE.wrap resultingInsertionEvents, ancestorCharacter, descendantCharacter)
  where
    errorMessage = mconcat [ "Lengths of sequences are not equal!\n"
                           , "Parent length: "
                           , show $ olength ancestorCharacter
                           , "\nChild length: "
                           , show $ olength descendantCharacter
                           ]
    (ancestorCharacter, descendantCharacter) = doAlignment ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
    (_, resultingDeletionEvents, resultingInsertionEvents) = foldlWithKey' f (0, mempty, mempty) $ zip (otoList ancestorCharacter) (otoList descendantCharacter)
    f (parentBaseIndex, deletions, insertions) _characterIndex (ancestorElement, descendantElement)
      -- Biological "Nothing" case
--      | nothingLogic                                       = (parentBaseIndex    , deletions , insertions )
      | ancestorElement == gap && descendantElement == gap = (parentBaseIndex    , deletions , insertions )
      -- Biological deletion event case
      | deletionEventLogic                                 = (parentBaseIndex + 1, deletions', insertions )
      -- Biological insertion event case
      | insertionEventLogic                                = (parentBaseIndex    , deletions , insertions')
      -- Biological substitution / non-substitution cases
      | ancestorElement == gap                             = (parentBaseIndex    , deletions , insertions )
      | otherwise {- Both not gap -}                       = (parentBaseIndex + 1, deletions , insertions )
      where
        deletions'          = parentBaseIndex `IS.insert` deletions
        insertions'         = IM.insertWith (<>) parentBaseIndex (Seq.singleton (descendantElement,edgeIdentifier)) insertions
        gap                 = getGapChar ancestorElement
--      containsGap char    = gap .&. char /= zeroBits
        insertionEventLogic =     ancestorElement == gap && descendantElement /= gap -- not (containsGap descendantElement)
        deletionEventLogic  =   descendantElement == gap && ancestorElement   /= gap --not (containsGap   ancestorElement)
{-        
        nothingLogic        =  (  ancestorElement == gap && containsGap descendantElement)
                            || (descendantElement == gap && containsGap   ancestorElement)
-}                             

-- | Transforms a node's decorations to include the implied alignment given a 'PsuedoCharacter' and sequence index.
deriveImpliedAlignment :: (EncodableDynamicCharacter s2, Foldable t, IANode' n s2, NodeConstraint n s1, Element s1 ~ Element s2)
                       => Int -> t PsuedoIndex -> n -> n
deriveImpliedAlignment sequenceIndex psuedoCharacterVal node = node `setHomologies'` leafHomologies
      where
        leafHomologies
          | length oldHomologies <= sequenceIndex = oldHomologies <> V.replicate (sequenceIndex - length oldHomologies) (constructDynamic []) <> pure leafAlignedChar
          | otherwise                             = oldHomologies V.// [(sequenceIndex, leafAlignedChar)]
          where
            oldHomologies = getHomologies' node
            
        leafSequence    = getForAlign node
        leafCharacter   = leafSequence V.! sequenceIndex
        leafAlignedChar = constructDynamic $ reverse result
        characterTokens = otoList leafCharacter
        gap             = getGapChar $ head characterTokens
        (_,_remaining,result)    = foldl' f (0 :: Int, characterTokens, []) psuedoCharacterVal
          where
            f (basesSeen, xs, ys) e
              | e == HardGap || e == SoftGap || e == DeletedBase || e == DelInsBase = (basesSeen    , xs , gap : ys )
              | otherwise                    = (basesSeen + 1, xs',       ys') 
              where
                xs' = fromMaybe []   $ tailMay xs 
                ys' = maybe ys (:ys) $ headMay xs

