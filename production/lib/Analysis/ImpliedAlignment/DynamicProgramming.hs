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

{-# LANGUAGE TypeFamilies #-}

-- TODO: Make an IppliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment.DynamicProgramming where

import           Analysis.General.NeedlemanWunsch hiding (SeqConstraint)
import           Analysis.ImpliedAlignment.Internal
import           Analysis.Parsimony.Binary.Internal (allOptimization)
import           Bio.Metadata
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node   hiding (children, name)
import           Bio.PhyloGraph.Solution
import           Bio.PhyloGraph.Tree
import           Bio.Character.Dynamic.Coded
import           Control.Applicative          ((<|>))
import           Data.Alphabet
import           Data.BitMatrix
import           Data.Foldable
import           Data.IntMap                  (IntMap, insert)
import qualified Data.IntMap            as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet            as IS
import           Data.Key
import           Data.Matrix.NotStupid hiding ((<|>),toList,trace)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Vector                  (Vector, imap)
import qualified Data.Vector             as V
import           Data.Vector.Instances        ()
import           Prelude               hiding (lookup)
import           Debug.Trace                  (trace)
import           Safe                         (tailMay)
import           Test.Custom hiding (children)


import Data.Function.Memoize
import Data.Bits


defMeta :: Vector (CharacterMetadata s)
defMeta = pure CharMeta
        { charType   = DirectOptimization
        , alphabet   = constructAlphabet []
        , name       = "DefaultCharacter"
        , isAligned  = False
        , isIgnored  = False
        , weight     = 1.0
        , stateNames = mempty
        , fitchMasks = trace "I suck so bad" $ undefined
        , rootCost   = 0.0
        , costs      = GeneralCost { indelCost = 2, subCost = 1 }
        }
  
newtype MutationAccumulator = Accum (IntMap Int, Int, Int, Int, Int, IntSet)

-- | Top level wrapper to do an IA over an entire solution
-- takes a solution
-- returns an AlignmentSolution
iaSolution :: SolutionConstraint r m f t n e s => r -> AlignmentSolution s
iaSolution inSolution = fmap (`iaForest` getMetadata inSolution) (getForests inSolution)

-- | Simple wrapper to do an IA over a forest
-- takes in a forest and some metadata
-- returns an alignment forest
iaForest :: (ForestConstraint f t n e s, Metadata m s) => f -> Vector m -> AlignmentForest s
iaForest inForest inMeta = fmap (`impliedAlign` inMeta) (trees inForest)

-- TODO: used concrete BitVector type instead of something more appropriate, like EncodableDynamicCharacter. 
-- This also means that there are a bunch of places below that could be using EDC class methods that are no longer.
-- The same will be true in DO.
-- | Function to perform an implied alignment on all the leaves of a tree
-- takes a tree and some metadata
-- returns an alignment object (an intmap from the leaf codes to the aligned sequence)
-- TODO: Consider building the alignment at each step of a postorder rather than grabbing wholesale
impliedAlign :: (TreeConstraint t n e s, Metadata m s) => t -> Vector m -> Alignment s
impliedAlign inTree inMeta = extractAlign numerated inMeta
    where
        numerated = numeratePreorder inTree (root inTree) inMeta (V.replicate (length inMeta) (0, 0))

extractAlign :: (TreeConstraint t n e s, Metadata m s) => (Counts, t) -> Vector m -> Alignment s
extractAlign (lens, numeratedTree) _inMeta = foldr (\n acc -> insert (fromJust $ getNodeIdx n numeratedTree) (makeAlignment n lens) acc) mempty allLeaves
    where
      allLeaves = filter (`nodeIsLeaf` numeratedTree) allNodes
      allNodes  = getNthNode numeratedTree <$> [0..numNodes numeratedTree -1]
      
-- | Simple function to generate an alignment from a numerated node
-- Takes in a Node
-- returns a vector of characters
makeAlignment :: (NodeConstraint n s) => n -> Counts -> Vector s
makeAlignment n seqLens = makeAlign (getFinalGapped n) (getHomologies n)
    where
        --makeAlign :: Vector s -> HomologyTrace -> Vector s
        makeAlign dynChar homologies = V.zipWith3 makeOne' dynChar homologies seqLens
        -- | /O((n+m)*log(n)), could be linear, but at least it terminates!
        makeOne' char homolog len = fromChars . toList $ result
          where
            result = V.generate (fst len + snd len) f
              where
                f i = case i `IM.lookup` mapping of
                        Nothing -> gapChar char
                        Just j  -> char `grabSubChar` j
                mapping = V.ifoldl' (\im k v -> IM.insert v k im) mempty homolog

-- | Main recursive function that assigns homology traces to every node
-- takes in a tree, a current node, a vector of metadata, and a vector of counters
-- outputs a resulting vector of counters and a tree with the assignments
-- TODO: something seems off about doing the DO twice here
numeratePreorder :: (TreeConstraint t n e s, Metadata m s) => t -> n -> Vector m -> Counts -> (Counts, t)
numeratePreorder initTree initNode inMeta curCounts
    | isLeafNode =  (curCounts, inTree)
    | leftOnly = --trace "left only case" $
        let
            (leftChildHomolog, counterLeft, insertionEvents) = alignAndNumerate curNode (fromJust $ leftChild curNode inTree) curCounts inMeta
            editedTreeLeft                                   = propagateIt inTree leftChildHomolog insertionEvents
            (leftRecurseCount, leftRecurseTree)              = numeratePreorder editedTreeLeft leftChildHomolog inMeta counterLeft
        in (leftRecurseCount, leftRecurseTree)
    | rightOnly = --trace "right only case" $
        let
            (rightChildHomolog, counterRight, insertionEvents) = alignAndNumerate curNode (fromJust $ rightChild curNode inTree) curCounts inMeta
            editedTreeRight                                    = propagateIt inTree rightChildHomolog insertionEvents
            (rightRecurseCount, rightRecurseTree)              = numeratePreorder editedTreeRight rightChildHomolog inMeta counterRight
        in (rightRecurseCount, rightRecurseTree)
    | otherwise =
        let
            ( leftChildHomolog, counterLeft , insertionEventsLeft)  = alignAndNumerate curNode (fromJust $ leftChild curNode inTree) curCounts inMeta
            backPropagatedTree                                      = backPropagation inTree leftChildHomolog insertionEventsLeft
            (leftRecurseCount, leftRecurseTree)                     = numeratePreorder backPropagatedTree leftChildHomolog inMeta counterLeft
            leftRectifiedTree                                       = leftRecurseTree `update` [leftChildHomolog] -- TODO: Check this order
            
            curNode'                                                = leftRectifiedTree `getNthNode` fromJust (getNodeIdx curNode leftRectifiedTree)
            {-(alignedRCur, alignedRight)                             = alignAndAssign curNode' (fromJust $ rightChild curNode' leftRectifiedTree)
            (rightChildHomolog, counterRight, insertionEventsRight) = numerateNode alignedRCur alignedRight leftRecurseCount inMeta-}
            (rightChildHomolog, counterRight, insertionEventsRight) = alignAndNumerate curNode' (fromJust $ rightChild curNode' leftRectifiedTree) counterLeft inMeta
            backPropagatedTree'                                     = backPropagation leftRectifiedTree rightChildHomolog insertionEventsRight
            rightRectifiedTree                                      = backPropagatedTree' `update` [rightChildHomolog]
            -- TODO: need another align and assign between the left and right as a last step?
            output                                                  = numeratePreorder rightRectifiedTree rightChildHomolog inMeta counterRight
        in output

        where
            -- Deal with the root case by making sure it gets default homologies
            inTree = if   nodeIsRoot initNode initTree
                     then initTree `update` [setHomologies initNode defaultHomologs]
                     else initTree
            curNode = getNthNode inTree . fromJust $ getNodeIdx initNode inTree
            curSeqs = getForAlign curNode
            isLeafNode = leftOnly && rightOnly
            leftOnly   = isNothing $ rightChild curNode inTree
            rightOnly  = isNothing $ leftChild  curNode inTree
            defaultHomologs = if V.length curSeqs == 0 then V.replicate (V.length inMeta) mempty
                                else imap (\i _ -> V.enumFromN 0 (numChars (curSeqs V.! i))) inMeta
            propagateIt tree child events = tree' `update` [child]
               where tree' = backPropagation tree child events
            alignAndNumerate n1 n2 = numerateNode n1Align n2Align
               where (n1Align, n2Align) = alignAndAssign n1 n2

            -- Simple wrapper to align and assign using DO
            --alignAndAssign :: NodeConstraint n s => n -> n -> (n, n)
            alignAndAssign node1 node2 = (setFinalGapped (fst allUnzip) node1, setFinalGapped (snd allUnzip) node2)
                where
                    final1 = getForAlign node1
                    final2 = getForAlign node2
                    allUnzip = V.unzip allDO
                    allDO = V.zipWith3 checkThenAlign final1 final2 inMeta
                    checkThenAlign s1 s2 m = if numChars s1 == numChars s2 then (s1, s2) else needlemanWunsch s1 s2 m

-- | Back propagation to be performed after insertion events occur in a numeration
-- goes back up and to the left, then downward
-- takes in a tree, a current node, and a vector of insertion event sets
-- return a tree with the insertion events incorporated
backPropagation :: TreeConstraint t n e s  => t -> n -> Vector IntSet -> t
backPropagation tree node insertionEvents
  | all onull insertionEvents = tree
  | otherwise =
    case parent node tree of
      Nothing       -> tree -- If at the root, do nothing
      Just myParent -> let nodeIsLeftChild = ((\n -> fromJust $ getNodeIdx n tree) <$> leftChild myParent tree) == getNodeIdx node tree
                           (tree', _)      = accountForInsertionEventsForNode tree myParent insertionEvents
                           tree''          = if   nodeIsLeftChild -- if we are the left child, then we only have to update the current node
                                              then tree'
                                              else
                                                case leftChild myParent tree of -- otherwise we need to propagate down but not at the current node
                                                  Nothing   -> tree'
                                                  Just left -> backPropagationDownward tree' left insertionEvents
                        in backPropagation tree'' myParent insertionEvents
  where
    backPropagationDownward treeContext subTreeRoot insertionEvents' = treeContext'''
      where
        (treeContext'  , subTreeRoot'  ) = accountForInsertionEventsForNode treeContext subTreeRoot insertionEvents' -- do the current node
        treeContext'' = case  leftChild subTreeRoot' treeContext' of -- first go down and to the left
                          Nothing -> treeContext'
                          Just x  -> backPropagationDownward treeContext'  x insertionEvents'
        treeContext'''= case rightChild subTreeRoot' treeContext'' of -- then go down and to the right
                          Nothing -> treeContext''
                          Just x  -> backPropagationDownward treeContext'' x insertionEvents'
        
-- | Account for any insertion events at the current node by updating homologies
-- depends on accountForInsertionEvents to do work
-- takes in a tree, current node, and a vector of insertion event sets
-- returns an updated tree and an updated node for convenience
accountForInsertionEventsForNode :: TreeConstraint t n e s  => t -> n -> Vector IntSet -> (t, n)
accountForInsertionEventsForNode tree node insertionEvents = (tree', node')
  where
    originalHomologies = getHomologies node
    mutatedHomologies  = V.zipWith accountForInsertionEvents originalHomologies insertionEvents
    node'              = setHomologies node mutatedHomologies
    tree'              = tree `update` [node']

-- | Accounts for insertion events by mutating homology trace
-- essentially increases each homology position by the number of insertions before it
-- takes in a homologies trace and a set of insertions
-- returns a mutated homologies trace
accountForInsertionEvents :: Homologies -> IntSet -> Homologies
accountForInsertionEvents homologies insertionEvents = V.generate (length homologies) f
  where
    f i = newIndexReference
      where
        newIndexReference          = oldIndexReference + insertionEventsBeforeIndex 
        oldIndexReference          = homologies V.! i
        insertionEventsBeforeIndex = olength $ IS.filter (<= oldIndexReference) insertionEvents

-- | Function to do a numeration on an entire node
-- given the ancestor node, ancestor node, current counter vector, and vector of metadata
-- returns a tuple with the node with homologies incorporated, and a returned vector of counters
numerateNode :: (NodeConstraint n s, Metadata m s) => n -> n -> Counts -> Vector m -> (n, Counts, Vector IntSet) 
numerateNode ancestorNode childNode initCounters inMeta = (setHomologies childNode homologs, counts, insertionEvents)
        where
            numeration = V.zipWith3 numerateOne (getForAlign ancestorNode) (getForAlign childNode) initCounters 
            (homologs, counts, insertionEvents) = V.unzip3 numeration

numerateOne :: SeqConstraint s => s -> s -> Counter -> (Homologies, Counter, IntSet)
numerateOne ancestorSeq descendantSeq (maxLen, initialCounter) = (descendantHomologies, (newLen, counter'), insertionEvents)
  where
    gapCharacter = gapChar descendantSeq
    newLen = max (numChars descendantSeq) maxLen

    descendantHomologies = V.generate (olength descendantSeq) g
     where
       g :: Int -> Int
       g i = fromMaybe (error "The apparently not impossible happened!") $ i `IM.lookup` mapping

    (Accum (mapping, counter', _, _, _, insertionEvents)) = ofoldl' f (Accum (mempty, initialCounter, 0, 0, 0, mempty)) descendantSeq
      where
        f (Accum (indexMapping, counter, i, ancestorOffset, childOffset, insertionEventIndicies)) _ -- Ignore the element parameter because the compiler can't make the logical type deduction :(
          -- Biological "Nothing" case
          | ancestorCharacter == gapCharacter && descendantCharacter == gapCharacter = Accum (insert i (i + childOffset    ) indexMapping, counter    , i + 1, ancestorOffset    , childOffset    , insertionEventIndicies)
          -- Biological deletion event case
          | ancestorCharacter /= gapCharacter && descendantCharacter == gapCharacter = Accum (insert i (i + childOffset + 1) indexMapping, counter + 1, i + 1, ancestorOffset    , childOffset + 1, insertionEventIndicies)
          -- Biological insertion event case
          | ancestorCharacter == gapCharacter && descendantCharacter /= gapCharacter = Accum (insert i (i + childOffset    ) indexMapping, counter + 1, i + 1, ancestorOffset + 1, childOffset + 1, (i + childOffset + 1) `IS.insert` insertionEventIndicies)
          -- Biological substitution or non-substitution case
          | otherwise {- Both not gap -}                                             = Accum (insert i (i + childOffset)     indexMapping, counter    , i + 1, ancestorOffset    , childOffset    , insertionEventIndicies)
          where
--          j = i + childOffset
            descendantCharacter    = fromJust $ safeGrab descendantSeq i
            ancestorCharacter = fromJust $ safeGrab ancestorSeq   i 

-- TODO: make sure a sequence always ends up in FinalGapped to avoid this decision tree
-- | Simple function to get a sequence for alignment purposes
getForAlign :: NodeConstraint n s => n -> Vector s
getForAlign n 
    | not . null $ getFinalGapped n = getFinalGapped n
    | not . null $ getPreliminary n = getPreliminary n 
    | not . null $ getEncoded     n = getEncoded n 
    | otherwise = mempty {-error "No sequence at node for IA to numerate"-}


-- | Function to do a numeration on an entire node
-- given the parent node, child node, current counter vector, and vector of metadata
-- returns a tuple with the node with homologies incorporated, and a returned vector of counters
comparativeNumeration :: (NodeConstraint n s) => n -> n -> Vector (IntMap Int) -> (n, Vector (IntMap Int), Vector IntSet)
comparativeNumeration parentNode childNode totalGapCounts = (setHomologies childNode homologs, totalGapCounts', insertionEvents)
  where
    totalGapCounts' = V.zipWith (IM.insert (0 {- We need a unique index for each node here or for each sewuence -})) childGapCounts totalGapCounts
    numeration      = V.zipWith characterNumeration (getForAlign parentNode) (getForAlign childNode)
    (homologs, childGapCounts, insertionEvents) = V.unzip3 numeration
                                                                                                                                            
characterNumeration :: SeqConstraint s => s -> s -> (Homologies, Int, IntSet)
characterNumeration = undefined
{-
characterNumeration ancestorSeq descendantSeq = (descendantHomologies, totalGaps, insertionEvents)                                          
  where                                                                                                                                     
    gap = gapChar descendantSeq                                                                                                             
    descendantHomologies = V.fromList . reverse $ indices                                                                                   
    YAA (_, totalGaps, indices, insertionEvents) = ofoldl' f (YAA (0, 0, [], mempty)) descendantSeq                                         
      where                                                                                                                                 
        f (YAA (i, counter, hList, insertionIndices)) _                                                                                     
          -- Biological "Nothing" case                                                                                                      
          | ancestorCharacter == gap && descendantCharacter == gap = YAA (i + 1, counter + 1,     hList,               insertionIndices)    
          -- Biological insertion event case                                                                                                
          | ancestorCharacter == gap && descendantCharacter /= gap = YAA (i + 1, counter    , i : hList, i `IS.insert` insertionIndices)    
          -- Biological deletion event case                                                                                                 
          | ancestorCharacter /= gap && descendantCharacter == gap = YAA (i + 1, counter + 1,     hList,               insertionIndices)    
          -- Biological substitution / non-substitution case                                                                                
          | otherwise {- Both not gap -}                           = YAA (i + 1, counter    , i : hList,               insertionIndices)    
          where                                                                                                                             
            descendantCharacter    = fromJust $ safeGrab descendantSeq i                                                                    
            ancestorCharacter      = fromJust $ safeGrab ancestorSeq   i 
-}

type AncestorDeletionEvents    = IntSet
type AncestorInsertionEvents   = IntSet
type DescendantInsertionEvents = IntSet

type MemoizedEvents = (DeletionEvents, InsertionEvents, PseudoCharacter)

{-
instance Monoid MemoizedEvents where
  mempty  = Memo (mempty, mempty, mempty)
  (Memo (a,b,c)) `mappend` (Memo (x,y,z)) = Memo (a<>x, b<>y, c<>z)
-}

newtype DeletionEvents = DE IntSet
instance Monoid DeletionEvents where
  mempty = DE mempty
  (DE ancestorSet) `mappend` (DE descendantSet) = DE $ incrementedDescendantSet <> ancestorSet
    where
      incrementedDescendantSet = ofoldl' f descendantSet ancestorSet
      f acc anscestorIndex  = ofoldl' g mempty acc
        where
          g is descendantIndex
            | anscestorIndex <= descendantIndex = IS.insert (descendantIndex + 1) is
            | otherwise                         = IS.insert  descendantIndex      is

newtype InsertionEvents = IE (IntMap Int)
instance Monoid InsertionEvents where
  mempty = IE mempty
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (+) k v mapping

(>-<) :: InsertionEvents -> InsertionEvents -> InsertionEvents
(>-<) (IE ancestorMap) (IE descendantMap) = IE $ decrementedDescendantMap <> ancestorMap
    where
      decrementedDescendantMap = foldlWithKey' f descendantMap ancestorMap
      f acc i ansVal = foldlWithKey' g mempty acc
        where
         g im k v
           | i+1 == k  = IM.insert  i      (ansVal + v) im
           | i   <= k  = IM.insert (k - 1)  v           im
           | otherwise = IM.insert  k       v           im

data PsuedoIndex
   = OriginalBase
   | InsertedBase
   | HardGap
   | SoftGap
   deriving (Eq,Show)

type PseudoCharacter = Vector PsuedoIndex

numeration :: (Eq n, Metadata m s, TreeConstraint t n e s, IANode' n s) => m -> t -> t
numeration metadataStructure tree = tree `update` updatedLeafNodes
  where
    -- | Precomputations used for reference in the memoization
    rootNode        = root tree
    enumeratedNodes = enumerateNodes tree
    nodeCount       = length         enumeratedNodes
    rootIndex       = locateRoot     enumeratedNodes rootNode
    childMapping    = gatherChildren enumeratedNodes tree
    parentMapping   = gatherParents  childMapping    rootIndex

    -- | Memoized multi-directional tree traversal
    homologyMemoize :: Matrix MemoizedEvents
    homologyMemoize = matrix nodeCount nodeCount opt
      where
--        opt (i,j) | trace (mconcat ["opt (",show i,",",show j,")"]) False = undefined
        opt (i,j)
          -- Base case with root node
          | i == rootIndex && j == rootIndex  = rootNodeValue
          -- Is a non-root node
          | i == j                            = nonRootNodeValue
          -- Is a child of the root node
          | i == rootIndex                    = parentChildEdge -- rootChildEdge
          -- In the lower triangle, never referenced
          | i >  j                            = mempty
          -- Not a direct descendant
          | j `onotElem` (childMapping V.! i) = mempty
          -- Direct descendant edge
          | otherwise                         = parentChildEdge
          where

            -- In the root case there can be no deletion events at the root node.
            -- The insertion events present at the root node are the culmination of all insertion events over the whole tree.
            -- The PsuedoCharacter at the root node contains all insertion events of the whole tree, intercalated as SoftGaps.
            rootNodeValue = (mempty, allDescendantInsertions, rootPsuedoCharacter)
              where
                rootPsuedoCharacter = V.fromList seqList
                  where
                    -- Maybe chack for indicies after the length of the sequence.
                    IE insertionMapping = allDescendantInsertions
                    characterLength     = olength $ getForAlign rootNode
                    seqList             = foldMap f [0..characterLength - 1]
                    f i =
                      case i `lookup` insertionMapping of
                        Nothing -> [OriginalBase]
                        Just n  -> replicate n SoftGap <> [OriginalBase]

            nonRootNodeValue = trace (mconcat ["nonRootNodeValue"]) $ (ancestoralDeletions, parentInsertions, psuedoCharacter)
              where
                -- We mutate the the psuedo-character by replacing "soft gaps" with "hard gaps"
                -- at indices where insertione events happened.
                psuedoCharacter = V.fromList . reverse $ result
                  where
                    (_,_,result) = foldl' f (0, m, []) parentPsuedoCharacter
                    IE m = inserts
                    f (basesSeen, mapping, es) e =
                      case e of
                        OriginalBase -> (basesSeen + 1, mapping, e : es)
                        InsertedBase -> (basesSeen + 1, mapping, e : es)
                        HardGap      -> (basesSeen    , mapping, e : es)
                        SoftGap      ->
                          case basesSeen `lookup` mapping of
                            Nothing -> (basesSeen, mapping, e : es)
                            Just c  ->
                              if c > 0
                              then (basesSeen, IM.update (pure . pred) basesSeen mapping, InsertedBase : es)
                              else (basesSeen,                                   mapping,            e : es)

            -- Child of the root node
            -- The deletion events are derived from a pairwise comparison of the root character and the child character.
            -- The insertion events are the culmination of the insertion events from all the child's children.
            -- The PseudoCharacter is not yet defined
            -- rootChildEdge = (deletes, inserts >-< allDescendantInsertions, undefined)

            -- The deletion events are derived from a pairwise comparison of the parent character and the child character,
            -- joined with the deletion events from the ancestor edges of the rooted tree.
            -- The insertion events are the culmination of the insertion events from all the child's children,
            -- joined with the insertion events are derived from a pairwise comparison of the parent character and the child character.
            -- The PseudoCharacter is not yet defined
            parentChildEdge = (ancestoralDeletions <> deletes, inserts >-< allDescendantInsertions, initialPsuedoCharacter)
              where
                initialPsuedoCharacter
                  | j < firstSiblingIndex = (\(_,_,x) -> x) $ homologyMemoize ! (i                , i                )
                  | otherwise             = substituteHardGaps . (\(_,_,x) -> x) $ homologyMemoize ! (previousLeafIndex, previousLeafIndex)
                  where
                    -- Search for the previous leaf node by traversing down the "right" most edges of the previous sibling node.
                    previousLeafIndex =
                      case lastMay . takeWhile (<j) $ otoList siblingIndices of
                        Nothing -> parentMapping V.! j 
                        Just x  -> rightMostLeafIndex x
                    rightMostLeafIndex n = fromMaybe n . fmap rightMostLeafIndex . maximumMay $ childMapping V.! n
                    siblingIndices       = j `IS.delete` (childMapping V.! i)
                    firstSiblingIndex    = minimumEx siblingIndices
                -- We mutate the the psuedo-character by replacing "soft gaps" with "hard gaps"
                -- at indices where insertione events happened.
                substituteHardGaps psuedoCharacter = V.fromList . reverse $ result
                  where
                    (_, result) = foldl f (0, []) psuedoCharacter
                    IE mapping = inserts
                    f (basesSeen, es) e =
                      case e of
                        OriginalBase -> (basesSeen + 1,  e : es)
                        HardGap      -> (basesSeen    ,  e : es)
                        SoftGap      -> (basesSeen    ,  e : es)
                        InsertedBase ->
                          case basesSeen `lookup` mapping of
                            Nothing -> (basesSeen,        e : es)
                            Just _  -> (basesSeen,  HardGap : es)
            
            parentCharacter = fromMaybe (error "No parent Sequence!") . headMay . getForAlign $ enumeratedNodes V.! i
            childCharacter  = fromMaybe (error "No child sequence!" ) . headMay . getForAlign $ enumeratedNodes V.! j
            (ancestoralDeletions, parentInsertions, parentPsuedoCharacter) =
              trace (mconcat ["Accessing (",show $ parentMapping V.! j,",",show j,")"])
              $ homologyMemoize ! (parentMapping V.! j, j)
            (deletes, inserts)          = comparativeIndelEvents parentCharacter childCharacter metadataStructure
            allDescendantInsertions     = ofoldl' f mempty (childMapping V.! i)
              where
                f acc x = acc <> directChildInsertions
                  where
                    (_, directChildInsertions, _) = homologyMemoize ! (j, x)

--    updatedLeafNodes :: (NodeConstraint n s, IANode' n s) => [n]
    updatedLeafNodes = foldrWithKey f [] enumeratedNodes
      where
        f i n xs
          | n `nodeIsLeaf` tree = deriveImpliedAlignment i n homologyMemoize : xs
          | otherwise           = xs

--    deriveImpliedAlignment :: (EncodableDynamicCharacter s, NodeConstraint n s, IANode' n s, Show s) => Int -> n -> n
deriveImpliedAlignment _ _ _ | trace "deriveImpliedAlignment" False = undefined
deriveImpliedAlignment index node homologyMemoize = node `setHomologies'` pure (constructDynamic result)
      where
        (deletions, insertions, psuedoCharacter) = homologyMemoize ! (index, index)
        leafCharacter = fromMaybe (error "No leaf node sequence!") . headMay $ getForAlign node
--        gap           = gapChar leafCharacter
        characterTokens = otoList leafCharacter
        gap           = getGapChar $ head characterTokens
        result        = snd $ foldr f (characterTokens, []) psuedoCharacter
          where
            f e (xs, ys) =
              case e of
                OriginalBase -> (xs',       ys')
                InsertedBase -> (xs',       ys')
                HardGap      -> (xs , gap : ys )
                SoftGap      -> (xs , gap : ys )
              where
                ys' = maybe ys (:ys) $ headMay xs
                xs' = fromMaybe []   $ tailMay xs 

enumerateNodes :: TreeConstraint t n e s  => t -> Vector n
enumerateNodes tree = V.generate (numNodes tree) (getNthNode tree)

locateRoot :: Eq n => Vector n -> n -> Int
locateRoot ns n = fromMaybe 0 $ V.ifoldl' f Nothing ns
  where
    f acc i e 
      | e == n    = acc <|> Just i
      | otherwise = Nothing

gatherChildren :: (Eq n, TreeConstraint t n e s) => Vector n -> t -> Vector IntSet
gatherChildren enumNodes tree = V.generate (length enumNodes) f
  where
    f i = IS.fromList $ location <$> children'
      where
        children'  = children node tree
        node       = enumNodes V.! i
        location n = fromMaybe (-1) $ V.ifoldl' g Nothing enumNodes
          where
            g acc i e =
              if   n == e
              then acc <|> Just i
              else acc


gatherParents :: Vector IntSet -> Int -> Vector Int
gatherParents childrenMapping rootIndex = V.generate (length childrenMapping) f
  where
    f i = fromMaybe (-1) $ foldlWithKey' g Nothing childrenMapping
      where
        g acc k e
          | i `oelem` e = Just k
          | otherwise   = acc

{-
gatherSubtree :: Vector IntSet -> Int -> BitMatrix
gatherSubtree = undefined
-}

comparativeIndelEvents :: (Metadata m s, SeqConstraint s) => s -> s -> m -> (DeletionEvents, InsertionEvents)
comparativeIndelEvents ancestorCharacterUnaligned descendantCharacterUnaligned metadataStructure
  | olength ancestorCharacter /= olength descendantCharacter = error $ mconcat ["Lengths of sequences are not equal!\n", "Parent length: ", show $ olength ancestorCharacter, "\nChild length: ", show $ olength descendantCharacter]
  | otherwise                                    = (DE deletionEvents, IE insertionEvents)
  where
    (ancestorCharacter, descendantCharacter) =
      if olength ancestorCharacterUnaligned == olength descendantCharacterUnaligned
      then (ancestorCharacterUnaligned, descendantCharacterUnaligned)
      else needlemanWunsch ancestorCharacterUnaligned descendantCharacterUnaligned metadataStructure
--    deletionEvents  = mempty
--    insertionEvents = mempty -- E . IM.singleton 0 $ [ _descendantCharacter `indexChar` 0 ]
    (_,deletionEvents,insertionEvents) = ofoldl' f (0, mempty, mempty) [0 .. olength descendantCharacter - 1]
    f (baseIndex, deletions, insertions) index
      -- Biological "Nothing" case
      | ancestorStatic == gap && descendantStatic == gap = (baseIndex    ,                       deletions,                               insertions)
      -- Biological insertion event case
      | ancestorStatic == gap && descendantStatic /= gap = (baseIndex    ,                       deletions, IM.insertWith (+) baseIndex 1 insertions)
      -- Biological deletion event case
      | ancestorStatic /= gap && descendantStatic == gap = (baseIndex + 1, baseIndex `IS.insert` deletions,                               insertions)
      -- Biological substitution / non-substitution case
      | otherwise {- Both not gap -}                     = (baseIndex + 1,                       deletions,                               insertions)
      where
        gap              = getGapChar ancestorStatic
        ancestorStatic   = ancestorCharacter   `indexChar` index
        descendantStatic = descendantCharacter `indexChar` index
