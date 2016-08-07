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

-- TODO: Remove, used for Show instance of MemoPoint
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Make an ImpliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment.DynamicProgramming where

import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Analysis.ImpliedAlignment.Internal
import           Analysis.Parsimony.Binary.DirectOptimization
import           Bio.Metadata
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node     hiding (Node,children, name)
import           Bio.PhyloGraph.Solution
import           Bio.PhyloGraph.Tree     hiding (edges)
import           Bio.Character.Dynamic.Coded
import           Control.Applicative            ((<|>))
import           Data.Alphabet
import           Data.Bits
import           Data.Foldable
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.IntSet                    (IntSet)
import qualified Data.IntSet             as IS
import           Data.Key
import           Data.List                      (intercalate,transpose)
import           Data.List.Utility              (equalityOf)
import           Data.Matrix.NotStupid   hiding ((<|>),toList,trace,transpose)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import qualified Data.Tree               as Tree
import qualified Data.Sequence           as Seq
import           Data.Vector                     (Vector)
import qualified Data.Vector             as V
import           Data.Vector.Instances           ()
import           Prelude                 hiding  (lookup,zip,zipWith)
import           Safe                            (tailMay)
import           Text.Show                       (showListWith)

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

-- TODO: make sure a sequence always ends up in FinalGapped to avoid this decision tree
-- | Simple function to get a sequence for alignment purposes
getForAlign :: NodeConstraint n s => n -> Vector s
getForAlign n 
--    | not . null $ getFinalGapped         n = getFinalGapped n
    | not . null $ getPreliminaryUngapped n = getPreliminaryUngapped n 
--    | not . null $ getSingle              n = getSingle n 
    | not . null $ getEncoded     n         = getEncoded n 
    | otherwise = mempty {-error "No sequence at node for IA to numerate"-}

shiftDescendantInsertions :: Eq a => DeletionEvents -> InsertionEvents a -> InsertionEvents a
shiftDescendantInsertions localDeletions descendantInsertions = IE.wrap result
  where
    (_,_,result) = foldlWithKey' f (0, otoList localDeletions, mempty) $ IE.unwrap descendantInsertions
    f (inc,   [], acc) k v = (inc , [], acc <> IM.singleton (k + inc ) v)
    f (inc, x:xs, acc) k v
      | x <= k    = (inc + 1,   xs, acc <> IM.singleton (k + inc + 1) v)
      | otherwise = (inc    , x:xs, acc <> IM.singleton (k + inc    ) v)

--type AncestorDeletionEvents    = IntSet
--type AncestorInsertionEvents   = IntSet
--type DescendantInsertionEvents = IntSet

--type MemoizedEvents  = (DeletionEvents, InsertionEvents, PseudoCharacter, DeletionEvents, DeletionEvents, InsertionEvents)
data MemoizedEvents s
   = Memo
   { cumulativeDeletionEvents       :: DeletionEvents
   , cumulativeInsertionEvents      :: InsertionEvents (Element s)
   , currentPsuedoCharacter         :: PseudoCharacter
   , parentPsuedoCharacter          :: PseudoCharacter
   , localRelativeDeletionEvents    :: DeletionEvents
   , localNormalizedDeletionEvents  :: DeletionEvents
   , localRelativeInsertionEvents   :: InsertionEvents (Element s)
   , localNormalizedInsertionEvents :: InsertionEvents (Element s)
   , doAncestorCharacter            :: Maybe s
   , doDescendantCharacter          :: Maybe s
   }

instance (EncodableDynamicCharacter s, Show (Element s)) => Show (MemoizedEvents s) where
  show memo = unlines
      [ "Deletion Events"
      , ("  Relative  : "<>) . show . otoList $ localRelativeDeletionEvents   memo
      , ("  Normalized: "<>) . show . otoList $ localNormalizedDeletionEvents memo
      , ("  Cumulative: "<>) . show . otoList $ cumulativeDeletionEvents      memo
      , "Insertion Events"
      , ("  Relative  : "<>) . show . IM.assocs . IE.unwrap $ localRelativeInsertionEvents   memo
      , ("  Normalized: "<>) . show . IM.assocs . IE.unwrap $ localNormalizedInsertionEvents memo
      , ("  Cumulative: "<>) . show . IM.assocs . IE.unwrap $ cumulativeInsertionEvents      memo
      , maybe "" (("Aligned Ancestor:\n  "  <>) . renderDynamicCharacter) $ doAncestorCharacter   memo
      , maybe "" (("Aligned Descendant:\n  "<>) . renderDynamicCharacter) $ doDescendantCharacter memo
      , "Psuedo-character:"
      , ("  "<>) . concatMap show . toList $ parentPsuedoCharacter  memo
      , ("  "<>) . concatMap show . toList $ currentPsuedoCharacter memo
      ]


{-
newtype InsertionEvents = IE (IntMap Int) deriving (Show)
instance Monoid InsertionEvents where
  mempty = IE mempty
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (+) k v mapping

(>-<) :: InsertionEvents -> InsertionEvents -> InsertionEvents
(>-<) (IE ancestorMap) (IE descendantMap) = IE $ IM.unionWith (+) decrementedDescendantMap ancestorMap
    where
      decrementedDescendantMap = foldMapWithKey f descendantMap
      f k v = IM.singleton (k - decrement) v
        where
         toks      = takeWhile ((< k) . fst) $ IM.assocs ancestorMap
         decrement = sum $ g <$> toks
         g (k', v')
           | k' + v' >= k = k - k'
           | otherwise    = v'
-}

normalizeInsertions :: Eq a => PseudoCharacter -> InsertionEvents a -> InsertionEvents a
normalizeInsertions char inserts = IE.wrap $ IM.fromList normalizedInserts
  where
    (_,_,normalizedInserts) = foldlWithKey' f (0, IM.assocs $ IE.unwrap inserts, []) char
    f acc@(          _,       [],      _) _ _ = acc
    f     (baseCounter, (k,v):xs, result) i e
      | k == baseCounter = (baseCounter',       xs, (i,v):result)
      | otherwise        = (baseCounter', (k,v):xs,       result)
      where
        baseCounter'
          | e == HardGap || e == SoftGap || e == DeletedBase = baseCounter
          | otherwise                                        = baseCounter + 1

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

    showList = showListWith (\x -> (show x <>))

type PseudoCharacter = Vector PsuedoIndex

deriveImpliedAlignments :: (Eq n, FoldableWithKey f, TreeConstraint t n e s, IANode' n s, Metadata m s, Key f ~ Int, Show (Element s)) => f m -> t -> t 
deriveImpliedAlignments sequenceMetadatas tree = foldlWithKey' f tree sequenceMetadatas
  where
    f t k m
      | getType m /= DirectOptimization = t
      | otherwise                       = numeration k (getCosts m) t


numeration :: (Eq n, TreeConstraint t n e s, IANode' n s, Show (Element s)) => Int -> CostStructure -> t -> t
numeration sequenceIndex costStructure tree =
--    trace renderedTopology $
--    trace gapColumnRendering $
--    trace (inspectGaps [33] renderingTree) $
--    trace eventRendering $
    tree `update` (snd <$> updatedLeafNodes)
  where
    -- | Precomputations used for reference in the memoization
    rootNode        = root tree
    enumeratedNodes = enumerateNodes tree
    nodeCount       = length         enumeratedNodes
    rootIndex       = locateRoot     enumeratedNodes rootNode
    childMapping    = gatherChildren enumeratedNodes tree
    parentMapping   = gatherParents  childMapping

    eventRendering  = show $ renderingTree

    renderedTopology   = renderRenderingTreeTopology renderingTree
    gapColumnRendering = mconcat ["All gap columns: ", show $ olength allGapColumns, "/", show . length . currentPsuedoCharacter $ homologyMemoize ! (rootIndex, rootIndex), "\n  ", show $ otoList allGapColumns]

    allGapColumns   = getAllGapColumns $ (V.! sequenceIndex) . getHomologies' . snd <$> updatedLeafNodes

    renderingTree   = constructRenderingTree sequenceIndex rootIndex adjacencyList homologyMemoize
      where
        adjacencyList = V.zip (enumeratedNodes V.// updatedNodes) childMapping


    -- | Memoized multi-directional tree traversal
--    homologyMemoize :: SeqConstraint s => Matrix (MemoizedEvents s)
    homologyMemoize = matrix nodeCount nodeCount opt
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
                , parentPsuedoCharacter          = mempty
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
                    insertionMapping = IE.unwrap allDescendantInsertions
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

            nonRootNodeValue = homologyMemoize ! (parentMapping V.! j, j)

            -- The deletion events are derived from a pairwise comparison of the parent character and the child character,
            -- joined with the deletion events from the ancestor edges of the rooted tree.
            -- The insertion events are the culmination of the insertion events from all the child's children,
            -- joined with the insertion events are derived from a pairwise comparison of the parent character and the child character.
            -- The PseudoCharacter is not yet defined
            parentChildEdge =
                let
                  resultPoint =
                    Memo
                      { cumulativeDeletionEvents       = purgedAncestoralDeletions <> DE deletes
                      , cumulativeInsertionEvents      = inserts <^> rereferencedDescendantInsertions -- purgedDescendantInsertions -- allDescendantInsertions
                      , currentPsuedoCharacter         = psuedoCharacter
                      , parentPsuedoCharacter          = parentNodePsuedoCharacter
                      , localRelativeDeletionEvents    = DE deletes
                      , localNormalizedDeletionEvents  = DE $ ancestoralNodeDeletions `incrementDescendant` (DE deletes)
                      , localRelativeInsertionEvents   = inserts
                      , localNormalizedInsertionEvents = normalizeInsertions psuedoCharacter inserts
                      , doAncestorCharacter            = Just doA
                      , doDescendantCharacter          = Just doD
                      }
                in
                  resultPoint
 
              where
--                parentCharacter = getFinal       (enumeratedNodes V.! i) V.! sequenceIndex
                parentCharacter = getSingle      (enumeratedNodes V.! i) V.! sequenceIndex
                childCharacter  = getForAlign    (enumeratedNodes V.! j) V.! sequenceIndex
                memoPoint       = homologyMemoize ! (i, i)
                ancestoralNodeDeletions   = cumulativeDeletionEvents memoPoint
                parentNodePsuedoCharacter = currentPsuedoCharacter   memoPoint
--                   trace (mconcat ["Accessing (",show $ parentMapping V.! j,",",show j,")"]) $
                   
                (DE deletes, !inserts, doA, doD) = comparativeIndelEvents parentCharacter childCharacter costStructure

--                (IE incrementedInsertionEvents)  = inserts >-< rereferencedDescendantInsertions
                rereferencedDescendantInsertions = (\x -> trace (show x) x) $  shiftDescendantInsertions (DE deletes) allDescendantInsertions
{-
                purgedDescendantInsertions = IE . foldMapWithKey f $ (\(IE x) -> x) allDescendantInsertions
                  where
                    f k v
                      | k `onotElem` deletes = IM.singleton k v
                      | v /= 1               = IM.singleton k (v-1)
                      | otherwise            = mempty
-}
                purgedAncestoralDeletions = DE . ofoldMap f $ (\(DE x) -> x) ancestoralNodeDeletions
                  where
                    f e =
                      case takeWhile ((<=e) . fst) ins of
                        []      -> IS.singleton e
                        (k,v):_ -> if k + (length v) >= e + 1
                                   then mempty
                                   else IS.singleton e
                    ins = IM.assocs $ IE.unwrap inserts
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
                psuedoCharacter = let x = V.fromList $ reverse result
                                  in if null leftoverInsertions
                                     then x 
                                     else trace (mconcat ["(", show i,",", show j, ") Leftover insertions: ", show leftoverInsertions]) $
                                          x
                  where
                    (_,_,leftoverInsertions,result) = --trace (mconcat ["(",show i,",",show j, ") = ", show m, " c: ", show contextualPreviousPsuedoCharacter]) $
                      foldl' f (0, 0, m, []) contextualPreviousPsuedoCharacter2
                    m = IE.unwrap inserts
                    f _q@(pBasesSeen, consecutiveInsertions, remainingInsertions, es) e = -- (\x -> trace (show e <> show _q <> show x) x) $
                      case e of
                        OriginalBase ->
                                   if    pBasesSeen `oelem` deletes
                                   then (pBasesSeen + 1, 0                        , remainingInsertions, DeletedBase : es)
                                   else (pBasesSeen + 1, 0                        , remainingInsertions,           e : es)
                        InsertedBase ->
                                   if    pBasesSeen `oelem` deletes
                                   then (pBasesSeen + 1, consecutiveInsertions + 1, remainingInsertions,     HardGap : es)
                                   else (pBasesSeen + 1, consecutiveInsertions + 1, remainingInsertions,           e : es)
                        DeletedBase  -> (pBasesSeen    , 0                        , remainingInsertions,           e : es) -- conditionallyInsert
                        HardGap      -> (pBasesSeen    , 0                        , remainingInsertions,           e : es) -- maybe increment consecutive here too?
                        SoftGap      -> conditionallyInsert
                      where 
--                        conditionallyDelete 
--                          | basesSeen `oelem` deletes = (basesSeen + 1, remainingInsertions, DeletedBase : es)
--                          | otherwise                 = (basesSeen + 1, remainingInsertions,           e : es)
                        conditionallyInsert =
                          case insertIndices of
                            []      -> (pBasesSeen, 0                    , remainingInsertions   ,            e : es)
                            (k,_):_ -> (pBasesSeen, consecutiveInsertions, remainingInsertions' k, InsertedBase : es)
                          where
                            insertIndices = catMaybes $ (\x -> (\y -> (x,y)) <$> (x `lookup` remainingInsertions)) <$> validIndices
                            validIndices  = [lower..upper]
                            lower = pBasesSeen - consecutiveInsertions
                            upper = pBasesSeen
                        remainingInsertions' target = IM.update decrementRemaining target remainingInsertions
                          where
                            decrementRemaining x
                              | length x > 1 = Just $ Seq.drop 1 x
                              | otherwise    = Nothing


                contextualPreviousPsuedoCharacter2
                  | j < firstSiblingIndex = parentNodePsuedoCharacter
                  | otherwise             = modifiedPsuedoCharacter
                  where
                    -- Search for the previous leaf node by traversing down the "right" most edges of the previous sibling node.
                    modifiedPsuedoCharacter = V.fromList $ reverse hardGappedCharacter
                      where
                        previousSiblings        = takeWhile (<j) $ otoList siblingIndices
                        previousInsertionEvents = IE.unwrap $ foldMap f previousSiblings
                          where
                            f siblingIndex = cumulativeInsertionEvents $ homologyMemoize ! (i, siblingIndex)
                            
                        (_,_,hardGappedCharacter) = foldl g (0, previousInsertionEvents, []) parentNodePsuedoCharacter
                        g (basesSeen, mapping, es) e =
                          case e of
                            OriginalBase -> (basesSeen + 1, mapping, e : es)
                            InsertedBase -> (basesSeen + 1, mapping, e : es)
                            HardGap      -> (basesSeen    , mapping, e : es)
                            DeletedBase  -> (basesSeen    , mapping, e : es) -- conditionallyInsert
                            SoftGap      -> conditionallyInsert
                          where 
                            conditionallyInsert =
                              case basesSeen `lookup` mapping of
                                Nothing -> (basesSeen, mapping, e : es)
                                Just c  ->
                                  if length c > 0
                                  then (basesSeen, IM.update (pure . Seq.drop 1) basesSeen mapping, HardGap : es)
                                  else (basesSeen,                                         mapping,       e : es)

                            

                    siblingIndices       = j `IS.delete` (childMapping V.! i)
                    firstSiblingIndex    = minimumEx siblingIndices


{-
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
-}
{-
                      = substituteHardGaps . first getInsertions {- . (\e@(x,_) -> trace (show x) e) -}
                                         . fromMaybe (n, getPsuedoCharacter n)
                                         . fmap (id &&& 
                                         .
                    siblingIndices       = j `IS.delete` (childMapping V.! i)
                    firstSiblingIndex    = minimumEx siblingIndices
--                    getInsertions n      = (\(_,x,_) -> x) $ homologyMemoize ! (parentMapping V.! n, n)
                    getPsuedoCharacter n = currentPsuedoCharacter $ homologyMemoize ! (n, n)
-}

{-
                -- We mutate the the psuedo-character by replacing "soft gaps" with "hard gaps"
                -- at indices where insertione events happened.
                substituteHardGaps (parentChar, childChar) = zipWith f parentChar childChar
                  where
                    f p c =
                      case (p,c) of
                        (SoftGap     , InsertedBase) -> HardGap
--                        (OriginalBase, DeletedBase ) -> OriginalBase
--                        (InsertedBase, HardGap     ) -> InsertedBase
                        (           e, _           ) -> e
-}

            allDescendantInsertions = ofoldMap f (childMapping V.! j)
              where
                f x = cumulativeInsertionEvents $ homologyMemoize ! (j, x)

--    updatedLeafNodes :: (NodeConstraint n s, IANode' n s) => [n]
    updatedLeafNodes = filter ((`nodeIsLeaf` tree) . snd) updatedNodes

    updatedNodes
      | equalityOf id lengths = foldMapWithKey f enumeratedNodes
      | otherwise = error $ show lengths
      where
        lengths = foldMapWithKey g enumeratedNodes
        g i _ = (:[]) . length . currentPsuedoCharacter $ homologyMemoize ! (i,i)
        f i n =
          case otoList $ deletesToCompare `IS.intersection` insertsToCompare of
                    [] -> result
                    xs -> trace (mconcat [ "Error on edge ("
                                         , show $ parentMapping V.! i
                                         , ","
                                         , show i
                                         , "), overlapping insertion and deletion events: \n"
                                         , show xs
                                         ]
                                ) result
          where
            result = [(i, deriveImpliedAlignment i sequenceIndex homologyMemoize n)]
            resultPoint         = homologyMemoize ! (i,i)
            insertsToMutate     = IE.unwrap $ localNormalizedInsertionEvents resultPoint
            mutator k v         = IS.fromList $ ((k+).pred) <$> [1..(length v)] 
            insertsToCompare    = foldMapWithKey mutator insertsToMutate
            DE deletesToCompare = localNormalizedDeletionEvents resultPoint

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
--        DE deletions    = cumulativeDeletionEvents memoPoint
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
        (_,_remaining,result)    =
                                  foldl' f (0 :: Int, characterTokens, [])
--                        $ (trace (mconcat ["deriveImpliedAlignment ",show nodeIndex," ",show psuedoCharacter," ",show deletions]))
                          psuedoCharacter
          where
            f (basesSeen, xs, ys) e
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

comparativeIndelEvents :: (SeqConstraint s) => s -> s -> CostStructure -> (DeletionEvents, InsertionEvents (Element s), s ,s)
comparativeIndelEvents ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
  | olength ancestorCharacter /= olength descendantCharacter = error errorMessage
  | otherwise                                                = -- (\x -> trace (show x) x) $
                                                               (DE deletionEvents, IE.wrap insertionEvents, ancestorCharacter, descendantCharacter)
  where
    errorMessage = mconcat [ "Lengths of sequences are not equal!\n"
                           , "Parent length: "
                           , show $ olength ancestorCharacter
                           , "\nChild length: "
                           , show $ olength descendantCharacter
                           ]
    (ancestorCharacter, descendantCharacter) = doAlignment ancestorCharacterUnaligned descendantCharacterUnaligned costStructure
    (_,deletionEvents,insertionEvents)     = foldlWithKey' f (0, mempty, mempty) $ zip (otoList ancestorCharacter) (otoList descendantCharacter)
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
        insertions'         = IM.insertWith (<>) parentBaseIndex (Seq.singleton descendantElement) insertions
        gap                 = getGapChar ancestorElement
        containsGap char    = gap .&. char /= zeroBits
        insertionEventLogic =     ancestorElement == gap && not (containsGap descendantElement)
        deletionEventLogic  =   descendantElement == gap && not (containsGap   ancestorElement)
{-        
        nothingLogic        =  (  ancestorElement == gap && containsGap descendantElement)
                            || (descendantElement == gap && containsGap   ancestorElement)
-}                             


data RenderingNode a e = Node a [RenderingEdge e a]

data RenderingEdge e a = Edge e (RenderingNode a e)

data RenderingDecoration
   = Decoration
   { dEncoded             :: Maybe String
   , dSingle              :: Maybe String
   , dPreliminaryUngapped :: Maybe String
   , dPreliminaryGapped   :: Maybe String
   , dLeftAlignment       :: Maybe String
   , dRightAlignment      :: Maybe String
   , dFinalUngapped       :: Maybe String
   , dFinalGapped         :: Maybe String
   , dImpliedAlignment    :: Maybe String
   , dLocalCost           :: Double
   , dTotalCost           :: Double
   , dSecretIndex         :: Int
   } deriving (Eq)

instance (EncodableDynamicCharacter s, Show s, Show (Element s)) => Show (PeekTree s) where
  show = drawTreeMultiLine . showNode
    where
      showNode (Node decoration edges) = Node (show decoration) (showEdge <$> edges)
      showEdge (Edge datum      node ) = Edge (renderMemoizedEvents datum) (showNode node)

instance Show RenderingDecoration where
  show decoration = intercalate "\n" . (unwords ["Node (", show $ dSecretIndex decoration,")"] :) $ catMaybes renderings
    where
      renderings = mconcat [renderedCosts, renderedDecorations]
      renderedCosts =
        [  pure $ "LocalCost   " <> show (dLocalCost decoration)
        ,  pure $ "TotalCost   " <> show (dTotalCost decoration)
        ]
      renderedDecorations =
        [ g "Encoded                   " <$> f dEncoded
        , g "Single                    " <$> f dSingle
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

renderMemoizedEvents :: (EncodableDynamicCharacter s, Show s, Show (Element s)) => MemoizedEvents s -> String
renderMemoizedEvents = show 

-- | Neat 2-dimensional drawing of a tree.
drawTreeMultiLine :: RenderingNode String String-> String
drawTreeMultiLine = unlines . draw

draw :: RenderingNode String String -> [String]
draw (Node x xs) = lines x <> drawSubTrees xs
  where
    drawSubTrees  []              = []
    drawSubTrees  [Edge e n]      = renderEdge e <> indent "`- " "   " (draw n)
    drawSubTrees ((Edge e n): es) = renderEdge e <> indent "+- " "|  " (draw n) <> drawSubTrees es
    indent first other = zipWith (<>) (first : repeat other)
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
          , dSingle              = getFieldMay getSingle
          , dPreliminaryUngapped = getFieldMay getPreliminaryUngapped
          , dPreliminaryGapped   = getFieldMay getPreliminaryGapped
          , dLeftAlignment       = getFieldMay getLeftAlignment
          , dRightAlignment      = getFieldMay getRightAlignment
          , dFinalUngapped       = getFieldMay getFinal
          , dFinalGapped         = getFieldMay getFinalGapped
          , dImpliedAlignment    = getFieldMay getHomologies'
          , dLocalCost           = getLocalCost node
          , dTotalCost           = getTotalCost node
          , dSecretIndex         = nodeIndex
          }

        getFieldMay accessor = fmap renderDynamicCharacter $ charIndex `lookup` accessor node

renderInspectedGaps :: (EncodableDynamicCharacter s, Show (Element s)) => [(RenderingDecoration, MemoizedEvents s, RenderingDecoration)] -> String
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

inspectGapIndex :: (Eq (Element s)) => Int -> RenderingNode t (MemoizedEvents s) -> [(t, MemoizedEvents s, t)]
inspectGapIndex gapIndex rootNode = catMaybes $ nodeEventSites rootNode
  where
    nodeEventSites (Node parentDecoration edges) = concatMap edgeEventSite edges
      where
--        edgeEventSite :: Edge MemoizedEvents RenderingDecoration -> Maybe (RenderingDecoration, MemoizedEvents, RenderingDecoration)
        edgeEventSite (Edge datum node@(Node childDecoration _)) = siteMay : recursiveSites
          where
            DE xs = localNormalizedDeletionEvents  datum 
            ys    = IE.unwrap $ localNormalizedInsertionEvents datum 
            recursiveSites = nodeEventSites node
            siteMay
              | gapIndex `oelem` xs || isJust (gapIndex  `lookup` ys) = Just (parentDecoration, datum, childDecoration)
              | otherwise                                             = Nothing 
          
--inspectGaps :: [Int] -> PeekTree s -> String
inspectGaps :: (Functor t, Foldable t, EncodableDynamicCharacter s, Show (Element s)) => t Int -> RenderingNode RenderingDecoration (MemoizedEvents s) -> String
inspectGaps gapIndices renderingTree = unlines ["Tree toplogy:", "", renderedTreeTopology, "", renderedInspections]
  where
    renderedTreeTopology = renderRenderingTreeTopology renderingTree
    inspectionResults    = (\x -> (x, x `inspectGapIndex` renderingTree)) <$> gapIndices
    renderedInspections  = foldMap f inspectionResults
    f (i, []) = "No indel events for index: " <> show i <> "\n\n"
    f (i, xs) = "Indel events for index: "    <> show i <> "\n\n" <> (renderInspectedGaps xs) <> "\n"

renderRenderingTreeTopology :: RenderingNode RenderingDecoration t -> String
renderRenderingTreeTopology = Tree.drawTree . toStringTree
  where
    toStringTree (Node x edges) = Tree.Node (show $ dSecretIndex x) $ (\(Edge _ n) -> toStringTree n) <$> edges

getAllGapColumns :: (EncodableDynamicCharacter c, Foldable t) => t c -> IntSet
getAllGapColumns = foldMapWithKey f . transpose . fmap otoList . toList
  where
    f :: EncodableStaticCharacter c => Int -> [c] -> IntSet
    f k elements 
      | all (== gap) elements = IS.singleton k
      | otherwise             = mempty
      where
        gap = getGapChar $ head elements
