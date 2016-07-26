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

import           Analysis.ImpliedAlignment.Internal
import           Analysis.Parsimony.Binary.DirectOptimization
--import           Analysis.Parsimony.Binary.Internal (allOptimization)
import           Bio.Metadata
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node   hiding (Node,children, name)
import           Bio.PhyloGraph.Solution
import           Bio.PhyloGraph.Tree
import           Bio.Character.Dynamic.Coded
import           Control.Applicative          ((<|>))
--import           Control.Arrow                ((&&&))
import           Data.Alphabet
--import           Data.Bifunctor               (first)
--import           Data.BitMatrix
import Data.Bits
import           Data.Foldable
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap            as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet            as IS
import           Data.Key
import           Data.List.Utility            (equalityOf)
import           Data.Matrix.NotStupid hiding ((<|>),toList,trace)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Vector                  (Vector)
import qualified Data.Vector             as V
import           Data.Vector.Instances        ()
import           Prelude               hiding (lookup,zipWith)
import           Safe                         (tailMay)
--import           Test.Custom

import Data.List (intercalate)

import           Debug.Trace                  (trace)

defMeta :: Vector (CharacterMetadata s)
defMeta = pure CharMeta
        { charType   = DirectOptimization
        , alphabet   = constructAlphabet []
        , name       = "DefaultCharacter"
        , isAligned  = False
        , isIgnored  = False
        , weight     = 1.0
        , stateNames = mempty
        , fitchMasks = undefined
        , rootCost   = 0.0
        , costs      = GeneralCost { indelCost = 2, subCost = 1 }
        }
  
newtype MutationAccumulator = Accum (IntMap Int, Int, Int, Int, Int, IntSet)

-- | Top level wrapper to do an IA over an entire solution
-- takes a solution
-- returns an AlignmentSolution
iaSolution' :: (Eq n, SolutionConstraint r m f t n e s, IANode' n s, Show (Element s)) => r -> r
iaSolution' inSolution = inSolution `setForests` fmap (`iaForest'` getMetadata inSolution) (getForests inSolution)

-- | Simple wrapper to do an IA over a forest
-- takes in a forest and some metadata
-- returns an alignment forest
iaForest' :: (Eq n, FoldableWithKey k, ForestConstraint f t n e s, IANode' n s, Metadata m s, Key k ~ Int, Show (Element s)) => f -> k m -> f
iaForest' inForest inMeta = inForest `setTrees` fmap (deriveImpliedAlignments inMeta) (trees inForest)

{-
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
                    checkThenAlign s1 s2 m = if numChars s1 == numChars s2 then (s1, s2) else doAlignment s1 s2 m

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

-- | Function to do a numeration on an entire node
-- given the parent node, child node, current counter vector, and vector of metadata
-- returns a tuple with the node with homologies incorporated, and a returned vector of counters
comparativeNumeration :: (NodeConstraint n s) => n -> n -> Vector (IntMap Int) -> (n, Vector (IntMap Int), Vector IntSet)
comparativeNumeration parentNode childNode totalGapCounts = (setHomologies childNode homologs, totalGapCounts', insertionEvents)
  where
    totalGapCounts' = V.zipWith (IM.insert (0 {- We need a unique index for each node here or for each sewuence -})) childGapCounts totalGapCounts
    numeration      = V.zipWith characterNumeration (getForAlign parentNode) (getForAlign childNode)
    (homologs, childGapCounts, insertionEvents) = V.unzip3 numeration
-}
{-

characterNumeration :: SeqConstraint s => s -> s -> (Homologies, Int, IntSet)
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

-- TODO: make sure a sequence always ends up in FinalGapped to avoid this decision tree
-- | Simple function to get a sequence for alignment purposes
getForAlign :: NodeConstraint n s => n -> Vector s
getForAlign n 
--    | not . null $ getFinalGapped         n = getFinalGapped n
    | not . null $ getPreliminaryUngapped n = getPreliminaryUngapped n 
    | not . null $ getEncoded     n         = getEncoded n 
    | otherwise = mempty {-error "No sequence at node for IA to numerate"-}



type AncestorDeletionEvents    = IntSet
type AncestorInsertionEvents   = IntSet
type DescendantInsertionEvents = IntSet

--type MemoizedEvents  = (DeletionEvents, InsertionEvents, PseudoCharacter, DeletionEvents, DeletionEvents, InsertionEvents)
data MemoizedEvents s
   = Memo
   { cumulativeDeletionEvents       :: DeletionEvents
   , cumulativeInsertionEvents      :: InsertionEvents
   , currentPsuedoCharacter         :: PseudoCharacter
   , localRelativeDeletionEvents    :: DeletionEvents
   , localNormalizedDeletionEvents  :: DeletionEvents
   , localRelativeInsertionEvents   :: InsertionEvents
   , localNormalizedInsertionEvents :: InsertionEvents
   , doAncestorCharacter            :: Maybe s
   , doDescendantCharacter          :: Maybe s
   }

instance EncodableDynamicCharacter s => Show (MemoizedEvents s) where
  show memo = unlines
      [ "Deletion Events"
      , ("  Relative  : "<>) . show . otoList . unDE $ localRelativeDeletionEvents   memo
      , ("  Normalized: "<>) . show . otoList . unDE $ localNormalizedDeletionEvents memo
      , ("  Cumulative: "<>) . show . otoList . unDE $ cumulativeDeletionEvents      memo
      , "Insertion Events"
      , ("  Relative  : "<>) . show . IM.assocs . unIE $ localRelativeInsertionEvents   memo
      , ("  Normalized: "<>) . show . IM.assocs . unIE $ localNormalizedInsertionEvents memo
      , ("  Cumulative: "<>) . show . IM.assocs . unIE $ cumulativeInsertionEvents      memo
      , maybe "" (("Aligned Ancestor:\n  "  <>) . renderDynamicCharacter) $ doAncestorCharacter   memo
      , maybe "" (("Aligned Descendant:\n  "<>) . renderDynamicCharacter) $ doDescendantCharacter memo
      , "Psuedo-character:"
      , ("  "<>) . concat . toList $ show <$> currentPsuedoCharacter memo
      ]
    where
      unDE (DE x) = x
      unIE (IE x) = x

{-
instance Monoid MemoizedEvents where
  mempty  = Memo (mempty, mempty, mempty)
  (Memo (a,b,c)) `mappend` (Memo (x,y,z)) = Memo (a<>x, b<>y, c<>z)
-}

-- TODO: Use BitVectors here for efficency!
newtype DeletionEvents = DE IntSet deriving (Show)
instance Monoid DeletionEvents where
  mempty = DE mempty

  {- | /O(m)/ where m is sequence length

       When we have two Deletion Event collections and we want to merge them
       into a new, larger deletion event collection, we must take into account
       that one collection is of anscestoral events and the other of descendant
       events. There will likely be a shift in the indices' "frames of reference"
       which will require incrementation of the descendant deletion event
       collection.


       |> CASE 1 (simple)
       -=-=-=-=-=-=-=-=-

       Consider the comparison between the follwoing sequences:

       Anscestor:  GATTACA
       Descendant: GAACA
       Alignment:  GA--ACA
       Deletion Events: [2,3]

       Consider the comparison betwen the folowing sequences:

       Anscestor:  GAACA
       Descendant: GAAC
       Alignment:  GAAC-
       Deletion Events: [4]

       We must consider the total alignment history when merging the two deletion
       event collections so that the deletion events of the child have a reference
       frame to the root sequence

       Alignment History:
         Grandparent:  GATTACA
         Parent:       GA--ACA
         Child:        GA--AC-

       Deletion event collections:
          [2,3] <> [4] = [2,3,6]

       Grandparent:  GATTACA
       Child:        GA--AC-

       Note that the index of 4 on the righthand side is incremented by 2 to 6.
       This is because there are 2 indicies in the ancestor deletion event
       collection that are less than 4.


       |> CASE 2: (complex)
       -=-=-=-=-=-=-=-=-

       Consider the comparison between the follwoing sequences:

       Anscestor:  GATTACATA
       Descendant: GACATA
       Alignment:  GA---CATA
       Deletion Events: [2,3,4]

       Consider the comparison betwen the folowing sequences:

       Anscestor:  GACATA
       Descendant: GAAA
       Alignment:  GA-A-A
       Deletion Events: [2,4]

       When the descendant deletion event collection has a deletion event with
       an index that is a member of the acestor deletion event collection, the
       descendant index must be updated by the number of sequential elements in
       the ancestor deletion collection starting from the matching index.

       Alignment History:
         Grandparent:  GATTACATA
         Parent:       GA--ACATA
         Child:        GA----A-A

       Deletion event collections:
          [2,3,4] <> [2,4] = [2,3,4,5,7]

       Grandparent:  GATTACA
       Child:        GA--A-A

       Note that the index of 2 on the righthand side is incremented by 3 to 5.
       This is because there are 3 *consecutive* indicies in the ancestor
       deletion event collection starting at index 2.

       Note that the index of 4 on the righthand side is incremented by 3 to 7.
       This is because there are 2 indicies in the ancestor deletion event
       collection that are less than 4 *and* there is 1 *consecutive* index in
       the ancestor deletion event collection starting at index 4.

  -}
  
  as@(DE ancestorSet) `mappend` ds@(DE descendantSet) = DE . (ancestorSet <>) $ as `incrementDescendant` ds

incrementDescendant (DE ancestorSet) (DE descendantSet) = incrementedDescendantSet
  where
      (_,_,incrementedDescendantSet) = ofoldl' f (0, otoList ancestorSet, mempty) descendantSet
      f (counter, [], is) descendantIndex = (counter, [], (counter + descendantIndex) `IS.insert` is)
      f (counter, as, is) descendantIndex =
        case remaining of
           []   -> (counter', [], (counter' + descendantIndex) `IS.insert` is)
           x:xs ->
             if   x > descendantIndex
             then (counter'    , x:xs, (      counter' + descendantIndex) `IS.insert` is)
             else (counter' + 1,   xs, (inc + counter' + descendantIndex) `IS.insert` is)
        where
          (prev, remaining) = span (< descendantIndex) as
          counter' = length prev + counter
          inc = consecutiveLength remaining

          descendantIndex' = descendantIndex + counter
          incrementation   = consecutiveLength . drop (descendantIndex' - 1) $ otoList ancestorSet

          consecutiveLength :: (Eq a, Num a) => [a] -> Int
          consecutiveLength = g 0
            where
              g n       [] = n
              g n      [_] = n + 1
              g n (x:y:ys)
                | x+1 == y  = g (n+1) (y:ys)
                | otherwise = n + 1
 
{-
  (DE ancestorSet) `mappend` (DE descendantSet) = DE $ incrementedAncestorSet <> descendantSet
    where
      incrementedAncestorSet = ofoldl' f mempty ancestorSet
      f acc ancestorIndex = (ofoldl' g 0 descendantSet + ancestorIndex) `IS.insert` acc
        where
          g inc descendantIndex
            | descendantIndex <= ancestorIndex = inc + 1 
            | otherwise                        = inc
-}
newtype InsertionEvents = IE (IntMap Int) deriving (Show)
instance Monoid InsertionEvents where
  mempty = IE mempty
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (+) k v mapping

(>-<) :: InsertionEvents -> InsertionEvents -> InsertionEvents
--(>-<) = (<>)
{--}
(>-<) (IE ancestorMap) (IE descendantMap) = IE $ IM.unionWith (+) decrementedDescendantMap ancestorMap
    where
      decrementedDescendantMap = foldMapWithKey f descendantMap
      f k v = IM.singleton (k - decrement) v
        where
         toks      = takeWhile ((<= k) . fst) $ IM.assocs ancestorMap
         decrement = sum $ snd <$> toks
{--}
{-
(>-<) (IE ancestorMap) (IE descendantMap) = IE $ decrementedDescendantMap <> ancestorMap
    where
      decrementedDescendantMap = foldlWithKey' f descendantMap ancestorMap
      f acc i ansVal = foldlWithKey' g mempty acc
        where
         g im k v
           | i+1 == k  = IM.insert  i      (ansVal + v) im
           | i   <= k  = IM.insert (k - 1)  v           im
           | otherwise = IM.insert  k       v           im
-}

normalizeInsertions :: PseudoCharacter -> InsertionEvents -> InsertionEvents
normalizeInsertions char (IE inserts) = IE $ IM.fromList normalizedInserts
  where
    (_,_,normalizedInserts) = foldlWithKey' f (0, IM.assocs inserts, []) char
    f acc@(baseCounter,       [], result) _ _ = acc
    f     (baseCounter, (k,v):xs, result) i e
      | k == baseCounter = (baseCounter',       xs, (i,v):result)
      | otherwise        = (baseCounter', (k,v):xs,       result)
      where
        baseCounter'
          | e == HardGap || e == SoftGap = baseCounter
          | otherwise                    = baseCounter + 1

data PsuedoIndex
   = OriginalBase
   | InsertedBase
   | DeletedBase
   | HardGap
   | SoftGap
   deriving (Eq)

instance Show PsuedoIndex where
    show OriginalBase = "O"
    show InsertedBase = "I"
    show DeletedBase  = "D"
    show HardGap      = "-"
    show SoftGap      = "~"

type PseudoCharacter = Vector PsuedoIndex

deriveImpliedAlignments :: (Eq n, FoldableWithKey f, TreeConstraint t n e s, IANode' n s, Metadata m s, Key f ~ Int, Show (Element s)) => f m -> t -> t 
deriveImpliedAlignments sequenceMetadatas tree = foldlWithKey' f tree sequenceMetadatas
  where
    f t k m
      | getType m /= DirectOptimization = t
      | otherwise                       = numeration k (getCosts m) t


numeration :: (Eq n, TreeConstraint t n e s, IANode' n s, Show (Element s)) => Int -> CostStructure -> t -> t
numeration sequenceIndex costStructure tree = -- trace (unlines $ (renderInspectedGaps . (`inspectGapIndex` renderingTree)) <$> [10,11]) $
--                                               trace eventRendering $
                                              tree `update` (snd <$> updatedLeafNodes)
  where
    -- | Precomputations used for reference in the memoization
    rootNode        = root tree
    enumeratedNodes = enumerateNodes tree
    nodeCount       = length         enumeratedNodes
--    rootIndex       = locateRoot'    parentMapping
    rootIndex       = locateRoot     enumeratedNodes rootNode
--    rootIndex       = locateRoot     enumeratedNodes rootNode tree
    childMapping    = gatherChildren enumeratedNodes tree
    parentMapping   = gatherParents  childMapping

    eventRendering  = show $ renderingTree

    renderingTree   = constructRenderingTree sequenceIndex rootIndex adjacencyList homologyMemoize
      where
        adjacencyList = V.zip (enumeratedNodes V.// updatedLeafNodes) childMapping

--    showIt = let !x = updatedLeafNodes
--             in x `seq` trace (show $ (\(y,_,_) -> y) <$> homologyMemoize) x

    -- | Memoized multi-directional tree traversal
--    homologyMemoize :: SeqConstraint s => Matrix (MemoizedEvents s)
    homologyMemoize = {- (\x -> trace (show x) x) $ -} matrix nodeCount nodeCount opt
      where
--        opt (i,j) | trace (mconcat ["opt (",show i,",",show j,")"]) False = undefined
        opt (i,j)
          -- The root node (base case)
          | i == rootIndex && j == rootIndex = -- (\x -> trace ("ROOT: " <> show x) x)
                                               rootNodeValue
          -- A non-root node
          | i == j                           = -- (\e@(_,x,y) -> trace (mconcat ["opt(", show i,",",show j,") ",show x," ",show y]) e)
                                               nonRootNodeValue
          -- An edge in the tree
          | j `oelem` (childMapping V.! i)   = -- (\e@(_,x,_) -> trace (mconcat ["opt(", show i,",",show j,") ", show x]) e)
                                               parentChildEdge
          -- Neither a node nor an edge
          | otherwise                        = undefined
          where

            -- In the root case there can be no deletion events at the root node.
            -- The insertion events present at the root node are the culmination of all insertion events over the whole tree.
            -- The PsuedoCharacter at the root node contains all insertion events of the whole tree, intercalated as SoftGaps.
            rootNodeValue =
                Memo
                { cumulativeDeletionEvents       = mempty
                , cumulativeInsertionEvents      = allDescendantInsertions
                , currentPsuedoCharacter         = rootPsuedoCharacter
                , localRelativeDeletionEvents    = mempty
                , localNormalizedDeletionEvents  = mempty
                , localRelativeInsertionEvents   = mempty
                , localNormalizedInsertionEvents = mempty
                , doAncestorCharacter            = Nothing
                , doDescendantCharacter          = Nothing
                }
              where
                rootPsuedoCharacter = V.fromList seqList
                  where
                    -- Maybe check for indicies after the length of the sequence.
                    IE insertionMapping = allDescendantInsertions
                    characterLength     = olength $ getForAlign rootNode V.! sequenceIndex
                    seqList             = (<> trailingInsertions) $ foldMap f [0..characterLength - 1]
                    f k =
                      case k `lookup` insertionMapping of
                        Nothing -> [OriginalBase]
                        Just n  -> replicate n SoftGap <> [OriginalBase]
                    trailingInsertions =
                      case characterLength `lookup` insertionMapping of
                        Nothing -> []
                        Just n  -> replicate n SoftGap

            nonRootNodeValue = homologyMemoize ! (parentMapping V.! j, j)

            -- The deletion events are derived from a pairwise comparison of the parent character and the child character,
            -- joined with the deletion events from the ancestor edges of the rooted tree.
            -- The insertion events are the culmination of the insertion events from all the child's children,
            -- joined with the insertion events are derived from a pairwise comparison of the parent character and the child character.
            -- The PseudoCharacter is not yet defined
            parentChildEdge =
                Memo
                { cumulativeDeletionEvents       = purgedAncestoralDeletions <> DE deletes
                , cumulativeInsertionEvents      = inserts >-< purgedDescendantInsertions -- allDescendantInsertions
                , currentPsuedoCharacter         = psuedoCharacter
                , localRelativeDeletionEvents    = DE deletes
                , localNormalizedDeletionEvents  = DE $ ancestoralNodeDeletions `incrementDescendant` (DE deletes)
                , localRelativeInsertionEvents   = inserts
                , localNormalizedInsertionEvents = normalizeInsertions psuedoCharacter inserts
                , doAncestorCharacter            = Just doA
                , doDescendantCharacter          = Just doD
                }
              where
                parentCharacter = getFinal       (enumeratedNodes V.! i) V.! sequenceIndex
                childCharacter  = getForAlign    (enumeratedNodes V.! j) V.! sequenceIndex
                memoPoint       = homologyMemoize ! (i, i)
                ancestoralNodeDeletions   = cumulativeDeletionEvents memoPoint
                parentNodePsuedoCharacter = currentPsuedoCharacter   memoPoint
--                   trace (mconcat ["Accessing (",show $ parentMapping V.! j,",",show j,")"]) $
                   
                (DE deletes, !inserts, doA, doD) = comparativeIndelEvents parentCharacter childCharacter costStructure

                (IE incrementedInsertionEvents)  = inserts >-< purgedDescendantInsertions

                purgedDescendantInsertions = IE . foldMapWithKey f $ (\(IE x) -> x) allDescendantInsertions
                  where
                    f k v
                      | k `onotElem` deletes = IM.singleton k v
                      | v /= 1               = IM.singleton k (v-1)
                      | otherwise            = mempty

                purgedAncestoralDeletions = DE . ofoldMap f $ (\(DE x) -> x) ancestoralNodeDeletions
                  where
                    f e =
                      case takeWhile ((<=e) . fst) ins of
                        []      -> IS.singleton e
                        (k,v):_ -> if k + v >= e + 1
                                   then mempty
                                   else IS.singleton e
                    ins = IM.assocs $ (\(IE x) -> x) inserts
{-
                incrementedDeletionEvents = DE . IS.fromList $ ofoldMap f deletes
                  where
                    allInsertionEvents   = (\(_,IE x,_) -> x) $ homologyMemoize ! (rootIndex, rootIndex)
                    otherInsertionEvents = IM.differenceWith diffMay allInsertionEvents incrementedInsertionEvents
                      where
                        diffMay x y
                          | x - y <= 0 = Nothing
                          | otherwise  = Just $ x - y
                    f e = [e + otherInsertionsBefore e]

                    otherInsertionsBefore n = sum $ IM.filterWithKey (\k _ -> k <= n) otherInsertionEvents
-}                    
                psuedoCharacter = V.fromList $ reverse result
                  where
                    (_,_,result) = --trace (mconcat ["(",show i,",",show j, ") = ", show m, " c: ", show contextualPreviousPsuedoCharacter]) $
                      foldl f (0, m, []) contextualPreviousPsuedoCharacter
                    IE m = inserts
                    f q@(basesSeen, mapping, es) e = -- (\x -> trace (show e <> show q <> show x) x) $
                      case e of
                        OriginalBase -> conditionallyDelete
                        InsertedBase -> conditionallyDelete
                        HardGap      -> (basesSeen    , mapping, e : es)
                        SoftGap      -> conditionallyInsert
                        DeletedBase  -> conditionallyInsert
                      where 
                        conditionallyDelete 
                          | basesSeen `oelem` deletes = (basesSeen + 1, mapping, DeletedBase : es)
                          | otherwise                 = (basesSeen + 1, mapping,           e : es)
                        conditionallyInsert =
                          case basesSeen `lookup` mapping of
                            Nothing -> (basesSeen, mapping, e : es)
                            Just c  ->
                              if c > 0
                              then (basesSeen, IM.update (pure . pred) basesSeen mapping, InsertedBase : es)
                              else (basesSeen,                                   mapping,            e : es)

                contextualPreviousPsuedoCharacter
                  | j < firstSiblingIndex = parentNodePsuedoCharacter
                  | otherwise             = modifiedPsuedoCharacter
                  where
                    -- Search for the previous leaf node by traversing down the "right" most edges of the previous sibling node.
                    modifiedPsuedoCharacter =
                      case lastMay . takeWhile (<j) $ otoList siblingIndices of
                        Nothing -> getPsuedoCharacter i
                        Just x  -> substituteHardGaps (getPsuedoCharacter i, rightMostLeafPseudoCharacter x)
                    rightMostLeafPseudoCharacter n = 
                      case maximumMay $ childMapping V.! n of
                        Nothing -> getPsuedoCharacter n
                        Just x  -> substituteHardGaps (getPsuedoCharacter n, rightMostLeafPseudoCharacter x)
{-
                      = substituteHardGaps . first getInsertions {- . (\e@(x,_) -> trace (show x) e) -}
                                         . fromMaybe (n, getPsuedoCharacter n)
                                         . fmap (id &&& 
                                         .
-}
                    siblingIndices       = j `IS.delete` (childMapping V.! i)
                    firstSiblingIndex    = minimumEx siblingIndices
--                    getInsertions n      = (\(_,x,_) -> x) $ homologyMemoize ! (parentMapping V.! n, n)
                    getPsuedoCharacter n = currentPsuedoCharacter $ homologyMemoize ! (n, n)

                -- We mutate the the psuedo-character by replacing "soft gaps" with "hard gaps"
                -- at indices where insertione events happened.
                substituteHardGaps (parentPsuedoCharacter, childPsuedoCharacter) = zipWith f parentPsuedoCharacter childPsuedoCharacter
                  where
                    f p c =
                      case (p,c) of
                        (SoftGap     , InsertedBase) -> HardGap
                        (OriginalBase, DeletedBase ) -> OriginalBase
                        (InsertedBase, DeletedBase ) -> InsertedBase
                        (      _,            e) -> e

            allDescendantInsertions = ofoldl' f mempty (childMapping V.! j)
              where
                f acc x = acc <> directChildInsertions
                  where
                    directChildInsertions = cumulativeInsertionEvents $ homologyMemoize ! (j, x)
            
--    updatedLeafNodes :: (NodeConstraint n s, IANode' n s) => [n]
    updatedLeafNodes
      | equalityOf id lengths = foldrWithKey f [] enumeratedNodes
      | otherwise = error $ show lengths
      where
        lengths = foldMapWithKey g enumeratedNodes
        g i _ = (:[]) . length . currentPsuedoCharacter $ homologyMemoize ! (i,i)
        f i n xs
          | n `nodeIsLeaf` tree = (i, deriveImpliedAlignment i sequenceIndex homologyMemoize n) : xs
          | otherwise           = xs

deriveImpliedAlignment :: (EncodableDynamicCharacter s, NodeConstraint n s, IANode' n s, Show s, Show (Element s)) => Int -> Int -> Matrix (MemoizedEvents s) -> n -> n
-- deriveImpliedAlignment nodeIndex _ _ | trace ("deriveImpliedAlignment " <> show nodeIndex <> " " <> show psuedoCharacter) False = undefined
deriveImpliedAlignment nodeIndex sequenceIndex homologyMemoize node =
{-
                                                                      trace (unwords
                                                                            [ "Memo Index:"
                                                                            , show nodeIndex
                                                                            ,"\n"
                                                                            , "Deletion Events: "
                                                                            , show deletions
                                                                            , "\n"
                                                                            , "Input psuedo-character:"
                                                                            , show psuedoCharacter
                                                                            , "\n"
                                                                            , "Input leaf-character:"
                                                                            , show leafCharacter
                                                                            , "\n"
                                                                            , "Ouput character:"
                                                                            , show result
                                                                            , "\n"
                                                                            , "Actual length:"
                                                                            , show $ length result
                                                                            , "Expected length:"
                                                                            , show $ length psuedoCharacter
                                                                            , "Remaining tokens:"
                                                                            , show remaining
                                                                            ]) $
-}
{-
                                                                      trace (
                                                                        if length result == length psuedoCharacter 
                                                                        then ""
                                                                        else 
                                                                            unwords
                                                                            [ "Memo Index:"
                                                                            , show nodeIndex
                                                                            ,"\n"
                                                                            , "Deletion Events: "
                                                                            , show deletions
                                                                            , "\n"
                                                                            , "Input psuedo-character:"
                                                                            , show psuedoCharacter
                                                                            , "\n"
                                                                            , "Input leaf-character:"
                                                                            , show leafCharacter
                                                                            , "\n"
                                                                            , "Ouput character:"
                                                                            , show result
                                                                            , "\n"
                                                                            , "Actual length:"
                                                                            , show $ length result
                                                                            , "Expected length:"
                                                                            , show $ length psuedoCharacter
                                                                            , "Remaining tokens:"
                                                                            , show remaining
                                                                            ]) $
-}
                                                                      node `setHomologies'` leafHomologies
      where
        memoPoint       = homologyMemoize ! (nodeIndex, nodeIndex)
        DE deletions    = cumulativeDeletionEvents memoPoint
        psuedoCharacter = currentPsuedoCharacter   memoPoint
        leafHomologies
          | length oldHomologies <= sequenceIndex = oldHomologies <> V.replicate (sequenceIndex - length oldHomologies) (constructDynamic []) <> pure leafAlignedChar
          | otherwise                             = oldHomologies V.// [(sequenceIndex, leafAlignedChar)]
          where
            oldHomologies = getHomologies' node
            
        leafSequence    = {- trace (mconcat
                                 [ "opt ("
                                 , show nodeIndex
                                 , ","
                                 , show nodeIndex
                                 , ") "
                                 , show $ length psuedoCharacter
                                 , show deletions
                                 --," ", show psuedoCharacter
                                 ]) $
                          -}
                          getForAlign node
        leafCharacter   = leafSequence V.! sequenceIndex
        leafAlignedChar = constructDynamic $ reverse result
        characterTokens = otoList leafCharacter
        gap             = getGapChar $ head characterTokens
        (_,remaining,result)    =
--                                  foldl f (0, characterTokens, [])
                                  foldlWithKey f (0, characterTokens, [])
--                        $ (trace (mconcat ["deriveImpliedAlignment ",show nodeIndex," ",show psuedoCharacter," ",show deletions]))
                          psuedoCharacter
          where
            f (basesSeen, xs, ys) _k e
              | e == HardGap || e == SoftGap || e == DeletedBase = (basesSeen    , xs , gap : ys )
--              | basesSeen `oelem` deletions  = (basesSeen + 1, xs , gap : ys )
              | otherwise                    = (basesSeen + 1, xs',       ys') 
              where
                xs' = fromMaybe []   $ tailMay xs 
                ys' = maybe ys (:ys) $ headMay xs
                

enumerateNodes :: TreeConstraint t n e s  => t -> Vector n
enumerateNodes tree = {- trace ("Enumerated Nodes: " <> show x) -} x
  where
    !x = V.generate (numNodes tree) (getNthNode tree)

locateRoot :: Eq n => Vector n -> n -> Int
locateRoot ns n = fromMaybe 0 $ V.ifoldl' f Nothing ns
  where
    f acc i e = acc <|> if   e == n
                        then Just i
                        else Nothing

gatherChildren :: (Eq n, TreeConstraint t n e s) => Vector n -> t -> Vector IntSet
gatherChildren enumNodes tree = x -- trace ("Gathered children: " <> show x) x
  where
    !x = V.generate (length enumNodes) f
    f i = IS.fromList $ location <$> children'
      where
        children'  = children node tree
        node       = enumNodes V.! i
        location n = fromMaybe (-1) $ V.ifoldl' g Nothing enumNodes
          where
            g acc k e =
              if   n == e
              then acc <|> Just k
              else acc


gatherParents :: Vector IntSet -> Vector Int
gatherParents childrenMapping = {- trace ("Gathered parents: " <> show x) -} integrityCheck x
  where
    !x = V.generate (length childrenMapping) f
    f i = fromMaybe (-1) $ foldlWithKey' g Nothing childrenMapping
      where
        g acc k e
          | i `oelem` e = Just k
          | otherwise   = acc
    integrityCheck vectorOfParents =
      case foldl' h (0 :: Int) vectorOfParents of
        0 -> error "There was no parent found!"
        1 -> vectorOfParents
        n -> error $ "Could not find the parent for " <> show n <> " nodes: " <> show x
      where
        h acc e
          | e == -1   = acc + 1
          | otherwise = acc

comparativeIndelEvents :: (SeqConstraint s) => s -> s -> CostStructure -> (DeletionEvents, InsertionEvents, s ,s)
comparativeIndelEvents ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
  | olength ancestorCharacter /= olength descendantCharacter = error $ mconcat ["Lengths of sequences are not equal!\n", "Parent length: ", show $ olength ancestorCharacter, "\nChild length: ", show $ olength descendantCharacter]
  | otherwise                                    = -- (\x -> trace (show x) x) $
                                                   (DE deletionEvents, IE insertionEvents, ancestorCharacter, descendantCharacter)
  where
    (ancestorCharacter, descendantCharacter) = doAlignment ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
--    deletionEvents  = mempty
--    insertionEvents = mempty -- E . IM.singleton 0 $ [ _descendantCharacter `indexChar` 0 ]
    (_,_,deletionEvents,insertionEvents) = ofoldl' f (0, 0, mempty, mempty) [0 .. olength descendantCharacter - 1]
    f (parentBaseIndex, offset, deletions, insertions) characterIndex
      -- Biological "Nothing" case
      | ancestorStatic == gap && descendantStatic == gap = (parentBaseIndex    , offset,                             deletions,                                     insertions)
      -- Biological "Nothing" case
     {-
      | (ancestorStatic == gap && descendantStatic == gap) ||
        (ancestorStatic /= descendantStatic &&
           containsGap ancestorStatic &&
           containsGap descendantStatic
        )
                                                         = (parentBaseIndex    ,                             deletions,                                     insertions)
-}

      -- Biological insertion event case
--      | ancestorStatic == gap && descendantStatic /= gap = (parentBaseIndex    ,                             deletions, IM.insertWith (+) parentBaseIndex 1 insertions)

      | insertionEventLogic = (parentBaseIndex    , offset,                            deletions, IM.insertWith (+) parentBaseIndex 1 insertions)

      -- Biological deletion event case
      | ancestorStatic /= gap && descendantStatic == gap = (parentBaseIndex + 1, offset', parentBaseIndex `IS.insert` deletions,                                     insertions)
--      | ancestorStatic /= gap && descendantStatic == gap = (parentBaseIndex + 1, offset', (parentBaseIndex + offset') `IS.insert` deletions,                                     insertions)
--      | ancestorStatic /= gap && descendantStatic == gap = (parentBaseIndex + 1, offset', characterIndex `IS.insert` deletions,                                     insertions)
--      | ancestorStatic /= gap && descendantStatic == gap = (parentBaseIndex + 1, offset', (parentBaseIndex + length insertions) `IS.insert` deletions,                                     insertions)
--      | deletionEventLogic  = (parentBaseIndex + 1, parentBaseIndex `IS.insert` deletions,                                     insertions)

      -- Biological substitution / non-substitution case
      | otherwise {- Both not gap -}                     = (parentBaseIndex + 1, offset,                            deletions,                                     insertions)
      where
        gap              = getGapChar ancestorStatic
        ancestorStatic   = ancestorCharacter   `indexChar` characterIndex
        descendantStatic = descendantCharacter `indexChar` characterIndex
        containsGap char = gap .&. char /= zeroBits
        insertionEventLogic = ancestorStatic   == gap && not (containsGap descendantStatic)
        deletionEventLogic  = descendantStatic == gap && not (containsGap ancestorStatic)
        offset'
          | isJust $ parentBaseIndex `lookup` insertions = offset + 1
          | otherwise = offset
        insertions'         = foldMapWithKey g insertions
          where
            g k v
              | k >= parentBaseIndex = IM.singleton (k+1) v
              | otherwise            = IM.singleton k v

data RenderingNode a e = Node a [RenderingEdge e a]

data RenderingEdge e a = Edge e (RenderingNode a e)

data RenderingDecoration
   = Decoration
   { dEncoded             :: Maybe String
   , dPreliminaryUngapped :: Maybe String
   , dPreliminaryGapped   :: Maybe String
   , dLeftAlignment       :: Maybe String
   , dRightAlignment      :: Maybe String
   , dFinalUngapped       :: Maybe String
   , dFinalGapped         :: Maybe String
   , dImpliedAlignment    :: Maybe String
   , dLocalCost           :: Double
   , dTotalCost           :: Double
   } deriving (Eq)

instance EncodableDynamicCharacter s => Show (PeekTree s) where
  show = drawTreeMultiLine . showNode
    where
      showNode (Node decoration edges) = Node (show decoration) (showEdge <$> edges)
      showEdge (Edge datum      node ) = Edge (renderMemoizedEvents datum) (showNode node)

instance Show RenderingDecoration where
  show decoration = intercalate "\n" $ catMaybes renderings
    where
      renderings = mconcat [renderedCosts, renderedDecorations]
      renderedCosts =
        [  pure $ "LocalCost   " <> show (dLocalCost decoration)
        ,  pure $ "TotalCost   " <> show (dTotalCost decoration)
        ]
      renderedDecorations =
        [ g "Encoded                   " <$> f dEncoded
        , g "Preliminary Ungapped      " <$> f dPreliminaryUngapped
        , g "Preliminary Gapped        " <$> f dPreliminaryGapped
        , g "Left  Child-wise Alignment" <$> f dLeftAlignment
        , g "Right Child-wise Alignment" <$> f dRightAlignment
        , g "Final Ungapped            " <$> f dFinalUngapped
        , g "Final Gapped              " <$> f dFinalGapped
        , g "Implied Alignment         " <$> f dImpliedAlignment
        ]
      f x = x decoration
      g prefix shown = prefix <> ": " <> shown

renderDynamicCharacter :: EncodableDynamicCharacter c => c -> String
renderDynamicCharacter char
  | onull char = ""
  | otherwise  = concatMap f $ decodeDynamic defaultAlphabet char
  where
    symbolCount     = stateCount $ char `indexChar` 0
    symbols         = take symbolCount arbitrarySymbols
    defaultAlphabet
      | symbolCount == 5 = constructAlphabet ["A","C","G","T"]
      | otherwise        = constructAlphabet symbols

    f :: [String] -> String
    f [x] = x
    f ambiguityGroup = "[" <> concat ambiguityGroup <> "]"

    arbitrarySymbols :: [String]
    arbitrarySymbols = fmap pure . ('-' :) $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']  

renderMemoizedEvents :: MemoizedEvents s -> String
renderMemoizedEvents (Memo (DE totalDels) (IE totalIns) char (DE localRelDels) (DE localNormDels) (IE localRelIns) (IE localNormIns) _ _) =
    mconcat [renderedDeletionEvents, renderedInsertionEvents, renderedPsuedoCharacter]
  where
    renderedDeletionEvents  = renderEvents "Deletion  Events" (show <$> otoList localRelDels) (show <$> otoList localNormDels) (show <$> otoList totalDels) 
      -- unlines . ("Deletion  Events":) . fmap (("  "<>) . mconcat) . wordChunk 100 . (\x -> "[" : x <> ["]"]) . withCommas $ show <$> otoList  del
    renderedInsertionEvents = renderEvents "Insertion Events" (show <$> IM.assocs localRelIns) (show <$> IM.assocs localNormIns) (show <$> IM.assocs totalIns)
--    unlines . ("Insertion Events:":) . fmap (("  "<>) . mconcat) . wordChunk 100 . (\x -> "[" : x <> ["]"]) . withCommas $ show <$> IM.assocs totalIns
    renderedPsuedoCharacter = unlines . ("Psuedo Character:":) . fmap (("  "<>) . mconcat) . wordChunk 100 $ show <$> toList  char

    renderEvents label xs ys zs = unlines $ label : relative <> normalized <> cumulative
      where
        relative   = ("Relative:":)   $ formatting xs
        normalized = ("Normalized:":) $ formatting ys
        cumulative = ("Cumulative:":) $ formatting zs
        formatting = fmap (("  "<>) . mconcat) . wordChunk 100 . (\x -> "[" : x <> ["]"]) . withCommas

    withCommas     [] = []
    withCommas    [x] = [x]
    withCommas (x:xs) = (x <> ",") : withCommas xs

    wordChunk :: Int -> [[a]] -> [[[a]]]
    wordChunk size [] = []
    wordChunk    0 _  = []
    wordChunk size stream = z : wordChunk size zs
      where
        (z,zs) = chunk size stream
        chunk :: Int -> [[a]] -> ([[a]],[[a]])
        chunk n [] = ([],[])
        chunk 0 xs = ([],xs)
        chunk n (x:xs)
          | n - len < 0 = ( [], xs)
          | otherwise   = (x:y, ys)

                                          where
            (y,ys) = chunk (n-len) xs
            len    = length x
            
-- | Neat 2-dimensional drawing of a tree.
drawTreeMultiLine :: RenderingNode String String-> String
drawTreeMultiLine = unlines . draw

draw :: RenderingNode String String -> [String]
draw (Node x xs) = lines x <> drawSubTrees xs
  where
    drawSubTrees  []              = []
    drawSubTrees  [Edge e n]      = renderEdge e <> shift "`- " "   " (draw n)
    drawSubTrees ((Edge e n): es) = renderEdge e <> shift "+- " "|  " (draw n) <> drawSubTrees es
    shift first other = zipWith (<>) (first : repeat other)
    renderEdge edgeString = pad <> payload <> pad
      where
        pad     = ["|"]
        prefix  =  "| "
        payload = (prefix <>) <$> lines edgeString

type PeekTree s = RenderingNode RenderingDecoration (MemoizedEvents s)

constructRenderingTree :: (Eq n, NodeConstraint n s, IANode' n s)
                       => Int
                       -> Int
                       -> Vector (n, IntSet)
                       -> Matrix (MemoizedEvents s)
                       -> PeekTree s
--                       -> RenderingNode String String
constructRenderingTree charIndex rootIndex adjacentcyList memoMatrix = constructTree rootIndex
  where
--    constructTree :: Int -> PeekTree s
    constructTree nodeIndex = Node decoration subForest
      where
        (node, childIndices) = adjacentcyList V.! nodeIndex
        subForest = constructChild <$> otoList childIndices
        constructChild childIndex = Edge (memoMatrix ! (nodeIndex, childIndex)) (constructTree childIndex)
        decoration =
          Decoration
          { dEncoded             = getFieldMay getEncoded
          , dPreliminaryUngapped = getFieldMay getPreliminaryUngapped
          , dPreliminaryGapped   = getFieldMay getPreliminaryGapped
          , dLeftAlignment       = getFieldMay getLeftAlignment
          , dRightAlignment      = getFieldMay getRightAlignment
          , dFinalUngapped       = getFieldMay getFinal
          , dFinalGapped         = getFieldMay getFinalGapped
          , dImpliedAlignment    = getFieldMay getHomologies'
          , dLocalCost           = getLocalCost node
          , dTotalCost           = getTotalCost node
          }

        getFieldMay accessor = fmap renderDynamicCharacter $ charIndex `lookup` accessor node

renderInspectedGaps :: EncodableDynamicCharacter s => [(RenderingDecoration, MemoizedEvents s, RenderingDecoration)] -> String
renderInspectedGaps = unlines . fmap renderEventSite
  where
    indentLine  = ("  " <>)
    indentBlock = unlines . fmap indentLine . lines
    renderEventSite (x,y,z) = unlines [ "Parent Node\n"
                                      , indentBlock $ show x
                                      , "Edge Events\n"
                                      , indentBlock $ show y
                                      , "Child Node\n"
                                      , indentBlock $ show z
                                      ]

--inspectGapIndex :: Int -> PeekTree -> [(RenderingDecoration, MemoizedEvents s, RenderingDecoration)]
inspectGapIndex gapIndex rootNode = catMaybes $ nodeEventSites rootNode
  where
    nodeEventSites (Node parentDecoration edges) = concatMap edgeEventSite edges
      where
--        edgeEventSite :: Edge MemoizedEvents RenderingDecoration -> Maybe (RenderingDecoration, MemoizedEvents, RenderingDecoration)
        edgeEventSite (Edge datum node@(Node childDecoration _)) = siteMay : recursiveSites
          where
            DE xs = localNormalizedDeletionEvents  datum 
            IE ys = localNormalizedInsertionEvents datum 
            recursiveSites = nodeEventSites node
            siteMay
              | gapIndex `oelem` xs || isJust (gapIndex  `lookup` ys) = Just (parentDecoration, datum, childDecoration)
              | otherwise                                             = Nothing 
          
--inspectGaps :: [Int] -> 
{-
inspectGaps gapIndices renderingTree = renderedTreeTopology <> renderedInpsections
  where
    redneredTreeTopology = "No Tree Rendering\n\n"
    inspectionResults    = id *** (`inspectGapIndex` renderingTree) <$> gapIndices
    renderedInpsections  = foldMap f inspectionResults
    f (i, []) = "No indel events for index: " <> show i <> "\n\n"
    f (i, xs) = "Indel events for index: " show i <> "\n\n" <> (unlines $ renderInspectedGapsinspectionResults <$> xs) <> "\n"
  
-}
