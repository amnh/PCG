-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.InsertionEvents.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Core types for representing and accumulating insertion events.
-----------------------------------------------------------------------------

module Analysis.ImpliedAlignment.InsertionEvents.Internal where

import           Analysis.ImpliedAlignment.DeletionEvents
import           Data.Bifunctor       (bimap,second)
import           Data.Foldable
import           Data.IntMap          (IntMap)
import qualified Data.IntMap   as IM
import           Data.Key
import           Data.List            (intercalate, transpose)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence        (Seq,splitAt)
import qualified Data.Sequence as Seq
import           Prelude       hiding (lookup,splitAt,zip,zipWith)

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
newtype InsertionEvents a = IE (IntMap (Seq a)) deriving (Eq)

instance Eq a => Monoid (InsertionEvents a) where
  -- | This represent no insertionevents occurring on an edge
  mempty = IE mempty

  -- | This operator is valid /only/ when combineing sibling edges.
  --   For combining insertion events on the edge between grandparent and parent
  --   'p' with insertion events of edges between parent and one or more children
  --   `cEdges`, use the following: 'p <^> mconcat cEdges'.
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (flip (<>)) k v mapping

-- | This operator is used for combining an direct ancestoral edge with the
--   combined insertion events of child edges.
--
--   Pronounced <http://www.dictionary.com/browse/coalesce "coalesce"> operator.
(<^>) :: Eq a => InsertionEvents a -> InsertionEvents a -> InsertionEvents a
(<^>) (IE ancestorMap) (IE descendantMap) = IE . IM.fromList $ result <> remaining acc
  where
    (_, acc, result) = foldlWithKey f initialAccumulator descendantMap
    initialAccumulator = (0, initializeMutationIterator (IM.assocs ancestorMap), [])
    -- dec is the total number of inserted bases from the ancestoral insertion set that have been consumed.
    -- ok/v are orignal key/value pair for this tuple
    -- lies are locat insertion eventes to be placed in ov
    -- ek/v are the fold element's  key/value pair
    -- aies are ancestor insertion events
    -- ries are ancestor insertion events
    f (dec, iter, ries) ek ev =
      case getCurr iter of
        Nothing             -> (dec, Done, (ek - dec, ev):ries)
        Just (ok, ov) ->
          let len    = length ov
              newAns = (ok, getState iter)
              {- How many element of the ancestor insertion sequence must be consumed for
                 the ancestoral key `ok` and the decendant key `ek` to be equal?
         
                 The following equation represents the "shift backwards" to align the
                 insertion events, answering the question above.
         
                 We want to solve the equation ``` ek - dec - x = ok ``` to determine the
                 index `x` for the IntMap. Basic algebra shows us the solution is:
                 ``` x = ek - dec - ok ```
              -}
              ansMod = (ek - dec - ok, ev)
          in if ek - (dec + len) > ok
             then f (dec + len, next iter         , newAns:ries) ek ev
             else   (dec      , mutate ansMod iter,        ries)




-- | This operator is used for combining an direct ancestoral edge with the
--   combined insertion events of child edges.
--
--   Pronounced <http://www.dictionary.com/browse/coalesce "coalesce"> operator.
coalesce :: (Eq a, Foldable t) => DeletionEvents -> InsertionEvents a -> t (InsertionEvents a) -> InsertionEvents a
coalesce ancestorDeletions (IE ancestorMap) descendantEvents = IE . IM.fromList $ result <> remaining acc
  where
    IE descendantMap = mconcat $ toList descendantEvents
    (_, _, acc, result) = foldlWithKey f initialAccumulator descendantMap
    initialAccumulator = (0, otoList ancestorDeletions, initializeMutationIterator (IM.assocs ancestorMap), [])
    -- off is the offset for the descendant keys equal to
    --    the toral number of deletion events strictly less than the key
    ---   minus total number of inserted bases from the ancestoral insertion set that have been consumed.
    -- ok/v are orignal key/value pair for this tuple
    -- lies are locat insertion eventes to be placed in ov
    -- ek/v are the fold element's  key/value pair
    -- aies are ancestor insertion events
    -- ries are ancestor insertion events
    f (off, dels, iter, ries) ek ev =
      case dels of
        de:ds ->
            if   de < (ek + off)
            then f (off + 1, ds, iter, ries) ek ev
            else alpha
        []    -> alpha
      where
        alpha =
          case getCurr iter of
            Nothing -> (off, dels, next iter, (ek + off, ev):ries)
            Just (ok, ov) ->
              let len    = length ov
                  newAns = (ok, getState iter)
              {- How many element of the ancestor insertion sequence must be consumed for
                 the ancestoral key `ok` and the decendant key `ek` to be equal?
         
                 The following equation represents the "shift backwards" to align the
                 insertion events, answering the question above.
         
                 We want to solve the equation ``` ek - off - x = ok ``` to determine the
                 index `x` for the IntMap. Basic algebra shows us the solution is:
                 ``` x = ek - off - ok ```
              -}
                  ansMod = (ek + off - ok, ev)
              in if ek + off - len > ok
                 then f (off - len, dels, next iter         , newAns:ries) ek ev
                 else   (off      , dels, mutate ansMod iter,        ries)


-- | A nicer version of Show hiding the internal structure.
instance Show a => Show (InsertionEvents a) where
  show (IE xs) = mconcat
      [ "{"
      , intercalate "," $ render <$> kvs
      , "}"
      ]
    where
      kvs = IM.assocs xs
      render (k, v) = mconcat
          [ "("
          , show k
          , ","
          , renderedValue
          , ")"
          ]
        where
          unenquote = filter (\x -> x /= '\'' && x /= '"') 
          renderedValue
            | all singleChar shown = concatMap unenquote shown
            | otherwise            = show shown
            where
              singleChar = (1==) . length . unenquote
              shown = toList $ show <$> v

-- | Constructs an InsertionEvents collection from a structure of integral keys
-- and sequences of equatable elements.
fromList :: (Enum i, Eq a, Foldable t, Foldable t') => t (i, t' a) -> InsertionEvents a
fromList = IE . IM.fromList . fmap (fromEnum `bimap` toSeq) . toList
  where
    toSeq = Seq.fromList . toList

-- | Constructs an InsertionEvents collection from an IntMap of Sequences
wrap :: Eq a => IntMap (Seq a) -> InsertionEvents a
wrap = IE

-- | Extracts an IntMap of Sequences from an InsertionEvents collection.
unwrap :: Eq a => InsertionEvents a -> IntMap (Seq a)
unwrap (IE x) = x


-- INTERNAL STRUCTURES:


-- Convinience type alias for Key-Value Pairs.
-- Should not leave Internal module scope!
--   DO NOT export.
--   DO NOT use in exported function type signitures.
type KVP a = (Int, Seq a)

-- Used in the coalesce fold's accumulator.
-- Enforces invariants when consuming the ancestoral insertion events.
data MutationIterator a
   = Done
   | Curr (KVP a) (IntMap (Seq a)) [(KVP a)]

-- Takes a list of key-value pairs and produces a MutationIterator for consuming
-- the insertion events.
--
-- Assumes that keys are unique and in /ascending/ order!
initializeMutationIterator :: [KVP a] -> MutationIterator a
initializeMutationIterator xs =
  case xs of
    []   -> Done
    e:es -> Curr e mempty es

-- Moves the MutationIterator forward one element in the ordered insertion event
-- stream.
next :: MutationIterator a -> MutationIterator a
next  Done         = Done
next (Curr _ _ xs) = initializeMutationIterator xs

-- Takes a MutationIterator an returns the unconsumed key-value pairs
remaining :: MutationIterator a -> [KVP a]
remaining  Done              = []
remaining (Curr (k,v) im xs) =  (k, im `applyMutations` v):xs

getCurr :: MutationIterator a -> Maybe (Int, Seq a)
getCurr  Done             = Nothing
getCurr (Curr (k,v) im _) = Just (k,v)

mutate :: (Int, Seq a) -> MutationIterator a -> MutationIterator a
mutate     _  Done             = Done
mutate (i,e) (Curr (k,v) im xs) = Curr (k,v) im' xs
  where
    im' = IM.insertWith (<>) i e im

getState :: MutationIterator a -> Seq a
getState  Done             = mempty
getState (Curr (k,v) im _) = im `applyMutations` v


-- Takes an IntMap of insertion events localized to a seqence and applies them to
-- that sequence producing a longer sequence with the insertions inserted.
applyMutations :: IntMap (Seq a) -> Seq a -> Seq a
applyMutations im xs = (<> trailing) . foldMapWithKey f $ toList xs
  where
    f k v =
      case k `lookup` im of
        Nothing ->      Seq.singleton v
        Just x  -> x <> Seq.singleton v
    trailing = fromMaybe mempty $ (length xs) `lookup` im
