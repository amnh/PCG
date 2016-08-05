-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.InsertionEvents
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Core types for representing and accumulating insertion events.
-----------------------------------------------------------------------------

-- TODO: Maybe we don't need these language extensions?
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Analysis.ImpliedAlignment.InsertionEvents where

import           Data.Bifunctor       (bimap,second)
import           Data.Foldable
import           Data.IntMap          (IntMap)
import qualified Data.IntMap   as IM
import           Data.Key
import           Data.List            (intercalate, transpose)
import           Data.Monoid
import           Data.Sequence        (Seq)
import qualified Data.Sequence as Seq
import           Prelude       hiding (lookup,zip,zipWith)

{- |
  Represents a collection of insertion events. This collection may be indicative
  of the insertion events on a single edge, the accumulation of insertion events
  across sibling edges, or the cumulative insertion events of all edges below an
  edge.

  A collection of unique integral keys and mapped sequences of equatable elements.
  The sequence type should have /O(1)/ length calculation and efficient /O(log n)/
  insertion operation.

  The integral key represents the 0-based index of the base /before/ which the
  sequence should be inserted.

  May be monoidally combined to represent the cumulative insertion events from all
  out edges of a node.

  May be also be combined directionally to accululate out edges and an in edge of
  a node to represent all insertion events below the in edge.
-}
newtype InsertionEvents a = IE (IntMap (Seq a))

instance Eq a => Monoid (InsertionEvents a) where
  -- | This represent no insertionevents occurring on an edge
  mempty = IE mempty

  -- | This operator is valid /only/ when combineing sibling edges.
  --   For combining insertion events on the edge between grandparent and parent
  --   'p' with insertion events of edges between parent and one or more children
  --   `cEdges`, use the following: 'p <^> mconcat cEdges'.
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (<>) k v mapping

-- | This operator is used for combining an direct ancestoral edge with the
--   combined insertion events of child edges.
{-
(<^>) :: Eq a => InsertionEvents a -> InsertionEvents a -> InsertionEvents a
(<^>) (IE ancestorMap) (IE descendantMap) = IE $ IM.unionWith (+) decrementedDescendantMap ancestorMap
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

fromList :: (Enum i, Foldable t, Foldable t') => t (i, t' a) -> InsertionEvents a
fromList = IE . IM.fromList . fmap (fromEnum `bimap` toSeq) . toList
  where
    toSeq = Seq.fromList . toList

-- | A nicer version of Show hiding the internal structure.
instance Show a => Show (InsertionEvents a) where
  show (IE xs) = mconcat [ "{"
                         , intercalate "," $ render <$> kvs
                         , "}"
                         ]
    where
      kvs = IM.assocs xs
      render (k, v) = mconcat [ "("
                              , show k
                              , ","
                              , renderedValue
                              ]
        where
          renderedValue
            | all singleChar shown = mconcat shown
            | otherwise            = show shown
            where
              singleChar = (1==) . length
              shown = toList $ show <$> v
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

{-
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
          | e == HardGap || e == SoftGap || e == DeletedBase = baseCounter
          | otherwise                                        = baseCounter + 1
-}

{-
numeration :: (Eq n, TreeConstraint t n e s, IANode' n s, Show (Element s)) => Int -> CostStructure -> t -> t
numeration sequenceIndex costStructure tree =
    trace renderedTopology $
    trace gapColumnRendering $
--    trace (inspectGaps [33] renderingTree) $
    trace eventRendering $
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
                let
                  resultPoint =
                    Memo
                      { cumulativeDeletionEvents       = purgedAncestoralDeletions <> DE deletes
                      , cumulativeInsertionEvents      = inserts >-< rereferencedDescendantInsertions -- purgedDescendantInsertions -- allDescendantInsertions
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

                (IE incrementedInsertionEvents)  = inserts >-< rereferencedDescendantInsertions

                rereferencedDescendantInsertions = IE . foldMapWithKey f $ (\(IE x) -> x) allDescendantInsertions
                  where
                    f k v = IM.singleton (k + incVal) v
                      where
                        incVal = length . takeWhile (<=k) $ otoList deletes
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
                psuedoCharacter = let x = V.fromList $ reverse result
                                  in if null leftoverInsertions
                                     then x 
                                     else trace (mconcat ["(", show i,",", show j, ") Leftover insertions: ", show leftoverInsertions]) $
                                          x
                  where
                    (_,_,leftoverInsertions,result) = --trace (mconcat ["(",show i,",",show j, ") = ", show m, " c: ", show contextualPreviousPsuedoCharacter]) $
                      foldl' f (0, 0, m, []) contextualPreviousPsuedoCharacter2
                    IE m = inserts
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
                              | x > 1     = Just (x - 1)
                              | otherwise = Nothing


                contextualPreviousPsuedoCharacter2
                  | j < firstSiblingIndex = parentNodePsuedoCharacter
                  | otherwise             = modifiedPsuedoCharacter
                  where
                    -- Search for the previous leaf node by traversing down the "right" most edges of the previous sibling node.
                    modifiedPsuedoCharacter = V.fromList $ reverse hardGappedCharacter
                      where
                        previousSiblings = takeWhile (<j) $ otoList siblingIndices
                        IE previousInsertionEvents = foldMap f previousSiblings
                          where
                            f siblingIndex = cumulativeInsertionEvents $ homologyMemoize ! (i, siblingIndex)
                            
                        (_,_,hardGappedCharacter) = foldl g (0, previousInsertionEvents, []) parentNodePsuedoCharacter
                        g q@(basesSeen, mapping, es) e = -- (\x -> trace (show e <> show q <> show x) x) $
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
                                  if c > 0
                                  then (basesSeen, IM.update (pure . pred) basesSeen mapping, HardGap : es)
                                  else (basesSeen,                                   mapping,       e : es)

                            

                    siblingIndices       = j `IS.delete` (childMapping V.! i)
                    firstSiblingIndex    = minimumEx siblingIndices



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
--                        (OriginalBase, DeletedBase ) -> OriginalBase
--                        (InsertedBase, HardGap     ) -> InsertedBase
                        (           e, _           ) -> e

            allDescendantInsertions = ofoldl' f mempty (childMapping V.! j)
              where
                f acc x = acc <> directChildInsertions
                  where
                    directChildInsertions = cumulativeInsertionEvents $ homologyMemoize ! (j, x)
            
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
            IE insertsToMutate  = localNormalizedInsertionEvents resultPoint
            mutator k v         = IS.fromList $ ((k+).pred) <$> [1..v] 
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

-}               
