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

{-# LANGUAGE FlexibleContexts #-}

module Analysis.ImpliedAlignment.InsertionEvents.Internal where

import           Analysis.ImpliedAlignment.DeletionEvents       (DeletionEvents)
import           Data.Bifunctor         (bimap)
import           Data.Foldable
import           Data.IntMap            (IntMap)
import qualified Data.IntMap     as IM
import           Data.Key
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence          (Seq)
import qualified Data.Sequence   as Seq
import           Prelude         hiding (lookup,splitAt,zip,zipWith)
import           Test.QuickCheck hiding (output)

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
newtype InsertionEvents e = IE (IntMap (Seq e)) deriving (Eq)

instance (Arbitrary e, Eq e) => Arbitrary (InsertionEvents e) where
  arbitrary = do
    let gen = arbitrary
    keys <- fmap getNonNegative . getNonEmpty <$> (arbitrary :: Gen (NonEmptyList (NonNegative Int)))
    vals <- vectorOf (length keys) (listOf1 gen) 
    pure . IE . IM.fromList $ zipWith (\x y -> (x, Seq.fromList y)) keys vals

instance Eq e => Monoid (InsertionEvents e) where
  -- | This represent no insertionevents occurring on an edge
  mempty = IE mempty

  -- | This operator is valid /only/ when combineing sibling edges.
  --   For combining insertion events on the edge between grandparent and parent
  --   'p' with insertion events of edges between parent and one or more children
  --   `cEdges`, use the following: 'p <^> mconcat cEdges'.
  (IE lhs) `mappend` (IE rhs) = IE $ foldlWithKey' f lhs rhs
    where
      f mapping k v = IM.insertWith (flip (<>)) k v mapping


-- | A nicer version of Show hiding the internal structure.
instance Show e => Show (InsertionEvents e) where
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


{-
instance (Show e) => Show (InsertionEvents a e) where
  show (IE im) = enclose .intercalate "," . fmap (f . (show *** show . fmap (show . snd) . toList)) $ IM.assocs im
    where
      enclose x = mconcat ["{",x,"}"]
      f (a,b)   = mconcat ["(",a,",",b,")"]
-}

-- | This operator is used for combining an direct ancestoral edge with the
--   combined insertion events of child edges.
--
--   Pronounced <http://www.dictionary.com/browse/coalesce "coalesce"> operator.
(<^>) :: Eq e => InsertionEvents e -> InsertionEvents e -> InsertionEvents e
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
coalesce :: (Eq e, Foldable t) => DeletionEvents -> InsertionEvents e -> t (InsertionEvents e) -> InsertionEvents e
coalesce ancestorDeletions (IE ancestorMap) descendantEvents
  | any (<0) $ IM.keys descendantMap = error "A negative key value was created"
  | size output /= sum (size <$> toList descendantEvents) + size (IE ancestorMap) = error "Serious problem, size is not additive"
  | otherwise                        = output
  where
    output = IE . IM.fromList $ result <> remaining acc
    IE descendantMap    = mconcat $ toList descendantEvents
    (_, _, acc, result) = foldlWithKey f initialAccumulator descendantMap
    initialAccumulator  = (0, otoList ancestorDeletions, initializeMutationIterator (IM.assocs ancestorMap), [])

    -- off is the offset for the descendant keys equal to
    --    the toral number of deletion events strictly less than the key
    ---   minus total number of inserted bases from the ancestoral insertion set that have been consumed.
    -- ok/v are orignal key/value pair for this tuple
    -- lies are locat insertion eventes to be placed in ov
    -- ek/v are the fold element's  key/value pair
    -- aies are ancestor insertion events
    -- ries are ancestor insertion event
    f (off, dels, iter, ries) ek ev =
      case (getCurr iter, dels) of
                                 -- If there is no more mutation state from the ancestoral insertion events
                                 -- nor any more ancestoral deletion events we will simply apply the offest
                                 -- to the descendant key
        (Nothing     ,    []) -> (off, dels, next iter, (ek + off, ev):ries)
        (Nothing     , de:ds) -> if de < (ek + off)
                                 -- If there is no more mutation state from the ancestoral insertion events
                                 -- and the next deletion event is strictly less than
                                 -- the current decendant element's key after the offest is applied
                                 -- then we increment the offest by one and recurse on the current decentant KVP.
                                 then f (off + 1,   ds,      iter,                ries) ek ev
                                 -- If there is no more mutation state from the ancestoral insertion events
                                 -- and the next deletion event is *not* strictly less than
                                 -- the current decendant element's key after the offest is applied
                                 -- then we simply apply the offset.
                                 else   (off    , dels, next iter, (ek + off, ev):ries)
        (Just (ok,ov),    []) -> let
                                   len    = length ov
                                   newAns = (ok, getState iter)
                                 in
                                   if ek + off - len > ok
                                 -- If there is no more ancestoral deletion events
                                 -- and the next deletion event is *not* less than the current keys after the offest is applied
                                 -- then we increment the offest by one.
                                   then f (off - len, dels, next iter         , newAns:ries) ek ev
                                   else g off iter dels ries ok ov ek ev
          
        (Just (ok,ov), de:ds) -> if ok > de && de < ek + off
                                 then f (off + 1,   ds,      iter,                ries) ek ev
                                 else g off iter dels ries ok ov ek ev

    g off iter dels ries ok ov ek ev
        | ek + off       < ok =   (off      , dels, iter              , (ek+off,ev):ries)
        | ek + off - len > ok = f (off - len, dels, next iter         ,      newAns:ries) ek ev
        | otherwise           =   (off      , dels, mutate ansMod iter,             ries)
        where
          len    = length ov
          newAns = (ok, getState iter)
          ansMod = (ek + off - ok, ev)
          
        




-- | Constructs an InsertionEvents collection from a structure of integral keys
-- and sequences of equatable elements.
fromList :: (Eq e, Enum i, Foldable t, Foldable t') => t (i, t' e) -> InsertionEvents e
fromList = IE . IM.fromList . fmap (fromEnum `bimap` toSeq) . toList
  where
    toSeq = Seq.fromList . toList

fromEdgeMapping :: Eq e => e -> IntMap Int -> InsertionEvents e
fromEdgeMapping edgeToken mapping = IE $ f <$> mapping
  where
    f count = Seq.fromList $ replicate count edgeToken

-- | Constructs an InsertionEvents collection from an IntMap of Sequences
wrap :: Eq e => IntMap (Seq e) -> InsertionEvents e
wrap = IE

-- | Extracts an IntMap of Sequences from an InsertionEvents collection.
unwrap :: Eq e => InsertionEvents e -> IntMap (Seq e)
unwrap (IE x) = x

size :: InsertionEvents e -> Int
size (IE im) = sum $ length <$> im



-- INTERNAL STRUCTURES:


-- Convenience type alias for Key-Value Pairs.
-- Should not leave Internal module scope!
--   DO NOT export.
--   DO NOT use in exported function type signitures.
type KVP a = (Int, Seq a)

-- Used in the coalesce fold's accumulator.
-- Enforces invariants when consuming the ancestoral insertion events.
data MutationIterator a
   = Done
   | Curr (KVP a) (IntMap (Seq a)) [KVP a]
   deriving (Show)

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
getCurr  Done            = Nothing
getCurr (Curr (k,v) _ _) = Just (k,v)

mutate :: (Int, Seq a) -> MutationIterator a -> MutationIterator a
mutate     _  Done             = Done
mutate (i,e) (Curr (k,v) im xs) = Curr (k,v) im' xs
  where
    im' = IM.insertWith (<>) i e im

getState :: MutationIterator a -> Seq a
getState  Done             = mempty
getState (Curr (_,v) im _) = im `applyMutations` v


-- Takes an IntMap of insertion events localized to a seqence and applies them to
-- that sequence producing a longer sequence with the insertions inserted.
applyMutations :: IntMap (Seq a) -> Seq a -> Seq a
applyMutations im xs = (<> trailing) . foldMapWithKey f $ toList xs
  where
    f k v =
      case k `lookup` im of
        Nothing ->      Seq.singleton v
        Just x  -> x <> Seq.singleton v
    trailing = fromMaybe mempty $ length xs `lookup` im
