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

import           Data.Bifunctor       (bimap,second)
import           Data.Foldable
import           Data.IntMap          (IntMap)
import qualified Data.IntMap   as IM
import           Data.Key
import           Data.List            (intercalate, transpose)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
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
    f (dec,  Done                 , ries) ek ev = (dec, Done, (ek - dec, ev):ries)

    f (dec, (Last (ok,ov) lies     ), ries) ek ev
      | ek - (dec + len) > ok = (dec + len, Done              , old:(ek - dec, ev):ries)
      | otherwise             = (dec      , Last (ok,ov) lies',                    ries)
      where
        old   = (ok, applyLocalInsertionEvents lies ov)
        len   = length ov
        {- How many element of the ancestor insertion sequence must be consumed for
           the ancestoral key `ok` and the decendant key `ek` to be equal?
        
           The following equation represents the "shift backwards" to align the
           insertion events, answering the question above.
         
           We want to solve the equation ``` ek - dec - x = ok ``` to determine the
           index `x` for the IntMap. Basic algebra shows us the solution is:
           ``` x = ek - dec - ok ```
        -}
        lies' = IM.insertWith (<>) (ek - dec - ok) ev  lies

    f (dec, iter@(Curr (ok,ov) lies aies), ries) ek ev
      | ek - (dec + len) > ok = f (dec + len, next iter              , old:ries) ek ev
      | otherwise             =   (dec      , Curr (ok,ov) lies' aies,     ries)
      where
        old   = (ok, applyLocalInsertionEvents lies ov)
        len   = length ov
        lies' = IM.insertWith (<>) (ek - dec - ok) ev  lies

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





-- Convinience type alias for Key-Value Pairs.
-- Should not leave Internal module scope!
--   DO NOT export.
--   DO NOT use in exported function type signitures.
type KVP a = (Int, Seq a)

-- Used in the coalesce fold's accumulator.
-- Enforces invariants when consuming the ancestoral insertion events.
data MutationIterator a
   = Done
   | Last (KVP a) (IntMap (Seq a))
   | Curr (KVP a) (IntMap (Seq a)) [(KVP a)]

-- Takes a list of key-value pairs and produces a MutationIterator for consuming
-- the insertion events.
--
-- Assumes that keys are unique and in /ascending/ order!
initializeMutationIterator :: [KVP a] -> MutationIterator a
initializeMutationIterator xs =
  case xs of
    []   -> Done
    [e]  -> Last e mempty
    e:es -> Curr e mempty es

-- Moves the MutationIterator forward one element in the ordered insertion event
-- stream.
next :: MutationIterator a -> MutationIterator a 
next  Done         = Done
next (Last _ _)    = Done
next (Curr _ _ xs) = initializeMutationIterator xs

-- Takes a MutationIterator an returns the unconsumed key-value pairs
remaining :: MutationIterator a -> [KVP a]
remaining  Done              = []
remaining (Last (k,v) im)    = [(k, applyLocalInsertionEvents im v)]
remaining (Curr (k,v) im xs) =  (k, applyLocalInsertionEvents im v):xs

-- Takes an IntMap of insertion events localized to a seqence and applies them to
-- that sequence producing a longer sequence with the insertions inserted.
applyLocalInsertionEvents :: IntMap (Seq a) -> Seq a -> Seq a
applyLocalInsertionEvents im xs = (<> trailing) . foldMapWithKey f $ toList xs
  where
    f k v =
      case k `lookup` im of
        Nothing ->      Seq.singleton v
        Just x  -> x <> Seq.singleton v
    trailing = fromMaybe mempty $ (length xs) `lookup` im
