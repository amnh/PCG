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

{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}

-- TODO: Remove, used for Show instance of MemoPoint
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Make an ImpliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment.AlignmentContext where

import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Bio.Character.Dynamic.Coded
import           Data.Foldable
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.Key
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence                   (Seq)
import qualified Data.Sequence           as Seq
import           Data.Vector.Instances           ()
import           Prelude                 hiding  (lookup,zip,zipWith)
import           Text.Show                       (showListWith)

--import           Debug.Trace                  (trace)


data PsuedoIndex
   = OriginalBase
   | InsertedBase
   | DeletedBase
   | HardGap
   | SoftGap
   deriving (Eq)

type PseudoCharacter = [PsuedoIndex]

data AlignmentContext e
   = Context
   { insertionEvents :: InsertionEvents e
   , psuedoCharacter :: PseudoCharacter
   }

instance Show PsuedoIndex where
    show OriginalBase = "O"
    show InsertedBase = "I"
    show DeletedBase  = "D"
    show HardGap      = "-"
    show SoftGap      = "~"
    showList = showListWith (\x -> (show x <>))


--TODO: remove the actual character.

applyLocalEventsToAlignment :: EncodableDynamicCharacter c => c -> DeletionEvents -> InsertionEvents (Element c) -> AlignmentContext (Element c) -> AlignmentContext (Element c)
applyLocalEventsToAlignment parentCharacter localDeletionEvents localInsertionEvents alignmentContext =
    Context
       { insertionEvents = IE.wrap resultInserts
       , psuedoCharacter = resultChar
       }
  where
    (_,_,_,_,_,_leftoverInsertions, resultInserts, resultChar) = foldl' f initalAccumulator $ psuedoCharacter alignmentContext

    initalAccumulator = (0, 0, 0, otoList parentCharacter, IE.unwrap localInsertionEvents, IE.unwrap $ insertionEvents alignmentContext, mempty, mempty)

    f _q@(pBasesSeen, cBasesSeen, consecutiveInsertionsSkipped, charElements, remainingLocalInsertions, unappliedGlobalInsertions, is, es) e =
      case e of
        OriginalBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , 0                               , tail' charElements, remainingLocalInsertions, unappliedGlobalInsertions, is, DeletedBase : es)
                   else (pBasesSeen + 1, cBasesSeen + 1, 0                               , tail' charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,          e : es)
        InsertedBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , consecutiveInsertionsSkipped + 1, tail' charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,  HardGap : es)
                   else (pBasesSeen + 1, cBasesSeen + 1, consecutiveInsertionsSkipped + 1, tail' charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,        e : es)
        DeletedBase  -> (pBasesSeen    , cBasesSeen    , 0                               ,       charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,        e : es)
        HardGap      -> (pBasesSeen    , cBasesSeen    , 0                               ,       charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,        e : es) -- maybe increment consecutive here too?
        SoftGap      -> conditionallyInsert
      where
        conditionallyInsert =
          case pBasesSeen `lookup` remainingLocalInsertions of
            Nothing -> (pBasesSeen, cBasesSeen, 0                    , charElements, remainingLocalInsertions, unappliedGlobalInsertions, is,          e : es)
            Just bs ->
              let b = bs ! 0
              in
                case (cBasesSeen `lookup` unappliedGlobalInsertions) >>= (consecutiveInsertionsSkipped `lookup`) of
                  Nothing -> error "Inconsistent indexing"
                  Just c  ->
                    if b /= c
                    then (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1, charElements,                       remainingLocalInsertions,                                                  unappliedGlobalInsertions, is,                     e : es)
                    else (pBasesSeen, cBasesSeen + 1, 0                               , charElements, imRemove pBasesSeen 0 remainingLocalInsertions, imRemove pBasesSeen consecutiveInsertionsSkipped unappliedGlobalInsertions, is,          InsertedBase : es)

tail' :: [a] -> [a]
tail'    []  = []
tail' (_:xs) = xs

imRemove :: Int -> Int -> IntMap (Seq a) -> IntMap (Seq a)
imRemove k i im = IM.update f k im
  where
    f v
      | Seq.null v' = Nothing
      | otherwise   = Just v'
      where
        v' = as <> Seq.drop 1 bs
        (as, bs) = Seq.splitAt i v


