-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.AlignmentContext
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Analysis.ImpliedAlignment.AlignmentContext
  ( AlignmentContext(..)
  , PseudoCharacter
  , PseudoIndex()
  , applyLocalEventsToAlignment
  , deriveContextFromCharacter
  , isPseudoGap
  , readPseudoCharacter
  ) where

import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Data.Foldable
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import           Data.Key
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence                  (Seq)
import qualified Data.Sequence           as Seq
import           Data.Vector.Instances          ()
import           Prelude                 hiding (lookup,zip,zipWith)
import           Text.Show                      (showListWith)

-- import           Debug.Trace                  (trace)

data PseudoIndex
   = OriginalBase
   | InsertedBase
   | DeletedBase
   | DelInsBase
   | HardGap
   | SoftGap
   deriving (Eq)

type PseudoCharacter = [PseudoIndex]

data AlignmentContext e
   = Context
   { insertionEvents :: InsertionEvents e
   , pseudoCharacter :: PseudoCharacter
   }

instance Show PseudoIndex where
    show OriginalBase = "O"
    show InsertedBase = "I"
    show DeletedBase  = "D"
    show DelInsBase   = "X"
    show HardGap      = "-"
    show SoftGap      = "~"
    showList = showListWith (\x -> (show x <>))

instance Show e => Show (AlignmentContext e) where
  show ac = unlines
          [ "Alignment Context:"
          , ("  "<>) .           show $ insertionEvents ac
          , ("  "<>) . concatMap show $ pseudoCharacter ac
          ]

isPseudoGap :: PseudoIndex -> Bool
isPseudoGap OriginalBase = False
isPseudoGap InsertedBase = False
isPseudoGap _            = True

readPseudoCharacter :: String -> [PseudoIndex]
readPseudoCharacter ('O':xs) = OriginalBase : readPseudoCharacter xs
readPseudoCharacter ('I':xs) = InsertedBase : readPseudoCharacter xs
readPseudoCharacter ('D':xs) = DeletedBase  : readPseudoCharacter xs
readPseudoCharacter ('X':xs) = DelInsBase   : readPseudoCharacter xs
readPseudoCharacter ('-':xs) = HardGap      : readPseudoCharacter xs
readPseudoCharacter ('~':xs) = SoftGap      : readPseudoCharacter xs
readPseudoCharacter       _  = []

applyLocalEventsToAlignment :: (Eq e, Show e) => e -> DeletionEvents -> AlignmentContext e -> AlignmentContext e
applyLocalEventsToAlignment edgeIdentifier (DE localDeletionEvents) alignmentContext = --  (\x -> trace ("\nOutput\n"<>show x) x)
    Context
    { insertionEvents = IE.wrap resultInserts
    , pseudoCharacter = reverse resultChar
    }
  where
    (_, _, _, resultInserts, resultChar) = foldl' f initalAccumulator $ pseudoCharacter alignmentContext

    initalAccumulator = (0, 0, IE.unwrap $ insertionEvents alignmentContext, mempty, mempty)

    f _q@(pBasesSeen, cBasesSeen, unappliedGlobalInsertions, is, cs) pc =
      case pc of
        -- In the case of an original base, we might delete the base. If so we must decrement the resulting InsertionEvents. We place in a deleted base. This can probably be replaced with a HardGap.
        OriginalBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , unappliedGlobalInsertions, is, DeletedBase : cs)
                   else (pBasesSeen + 1, cBasesSeen + 1, unappliedGlobalInsertions, is,          pc : cs)

        -- In the case of an inserted base, we might delete the base. If so we must decrement the resulting InsertionEvents. We place in a HardGap.
        InsertedBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , unappliedGlobalInsertions, is, DelInsBase : cs)
                   else (pBasesSeen + 1, cBasesSeen + 1, unappliedGlobalInsertions, is,         pc : cs)

        -- Same logic in nesxt two cases, essentially the identifunction of the accumulator.
        DeletedBase  -> (pBasesSeen    , cBasesSeen    , unappliedGlobalInsertions, is,        pc : cs)
        DelInsBase   -> (pBasesSeen    , cBasesSeen    , unappliedGlobalInsertions, is,        pc : cs)
        
        HardGap      -> (pBasesSeen    , cBasesSeen    , imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.take 1 (unappliedGlobalInsertions ! pBasesSeen)) is,       pc : cs) -- maybe increment consecutive here too?

        -- The complicated case with indexing.
        SoftGap      ->
          case (pBasesSeen `lookup` unappliedGlobalInsertions) >>= (0 `lookup`) of
            Nothing    -> error ("Inconsistent indexing @ " <> show pBasesSeen <> ": " <> show unappliedGlobalInsertions <> show (insertionEvents alignmentContext))
--                        (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1,                        remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, is,                     e : es)
            Just e ->
              if e /= edgeIdentifier
              then (pBasesSeen, cBasesSeen    , imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.singleton e) is,                    pc : cs)
              else (pBasesSeen, cBasesSeen + 1, imRemove pBasesSeen 0 unappliedGlobalInsertions,                                    is,          InsertedBase : cs)

imAdd :: IS.Key -> Seq a -> IntMap (Seq a) -> IntMap (Seq a)
imAdd = IM.insertWith (flip (Seq.><) )

imRemove :: Int -> Int -> IntMap (Seq a) -> IntMap (Seq a)
imRemove k i = IM.update f k
  where
    f v
      | Seq.null v' = Nothing
      | otherwise   = Just v'
      where
        v' = as <> Seq.drop 1 bs
        (as, bs) = Seq.splitAt i v

-- | Derive an initial 'AlignmentContext' from a given character and
--   the 'InsertionEvents' contained in it's subtree.
deriveContextFromCharacter :: (Eq e, MonoFoldable t) => t -> InsertionEvents e -> AlignmentContext e
deriveContextFromCharacter char totalInsertionEvents =
    Context
    { insertionEvents = totalInsertionEvents
    , pseudoCharacter = resultingPseudoCharacter
    }
  where
    resultingPseudoCharacter
      | null leftOverInsertions = seqList
      | otherwise               = error "Leftover insertion events at root node. Out of range!"
      where
        insertionMapping = IE.unwrap totalInsertionEvents
        characterLength  = olength char
        seqList          = (<> trailingInsertions) $ foldMap f [0..characterLength - 1]
        f k =
          case k `lookup` insertionMapping of
            Nothing -> [OriginalBase]
            Just n  -> replicate (length n) SoftGap <> [OriginalBase]
            
        trailingInsertions =
          case characterLength `lookup` insertionMapping of
            Nothing -> []
            Just n  -> replicate (length n) SoftGap

        -- Check for indicies after the length of the sequence.
        leftOverInsertions = filter (outsideRange . fst) $ IM.assocs insertionMapping
          where
            outsideRange i = i < 0 || characterLength < i

{-
consistency :: Eq e => AlignmentContext e -> Bool
consistency ac = truth
  where
    m = IE.unwrap $ insertionEvents ac
    (_,_,truth) = foldl f (0,0,True) $ pseudoCharacter ac 

    f a@(_,_,False) _ = a
    f (basesSeen, consecutiveBases, result) e = 
      case e of
        OriginalBase -> (basesSeen + 1, 0                   , result)
        InsertedBase -> (basesSeen + 1, 0                   , result)
        DeletedBase  -> (basesSeen    , 0                   , result)
        DelInsBase   -> (basesSeen    , 0                   , result)
        HardGap      -> (basesSeen    , consecutiveBases + 1, isJust ((basesSeen `lookup` m) >>= (consecutiveBases `lookup`)))
        SoftGap      -> (basesSeen    , consecutiveBases + 1, isJust ((basesSeen `lookup` m) >>= (consecutiveBases `lookup`)))
    -}
