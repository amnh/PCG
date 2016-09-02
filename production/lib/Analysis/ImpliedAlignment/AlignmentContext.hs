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

module Analysis.ImpliedAlignment.AlignmentContext where

import           Analysis.ImpliedAlignment.DeletionEvents
import           Analysis.ImpliedAlignment.InsertionEvents
import qualified Analysis.ImpliedAlignment.InsertionEvents as IE (unwrap,wrap) 
import           Data.Foldable
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import           Data.Key
import           Data.List                      (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence                  (Seq)
import qualified Data.Sequence           as Seq
import           Data.Vector.Instances          ()
import           Prelude                 hiding (lookup,zip,zipWith)
import           Text.Show                      (showListWith)

import           Debug.Trace                  (trace)


data PsuedoIndex
   = OriginalBase
   | InsertedBase
   | DeletedBase
   | DelInsBase
   | HardGap
   | SoftGap
   deriving (Eq)

type PseudoCharacter = [PsuedoIndex]

data AlignmentContext a e
   = Context
   { insertionEvents :: InsertionEvents a e
   , psuedoCharacter :: PseudoCharacter
   }

instance Show PsuedoIndex where
    show OriginalBase = "O"
    show InsertedBase = "I"
    show DeletedBase  = "D"
    show DelInsBase   = "X"
    show HardGap      = "-"
    show SoftGap      = "~"
    showList = showListWith (\x -> (show x <>))

instance (Show a, Show e) => Show (AlignmentContext a e) where
  show ac = unlines
          [ "Alignment Context:"
          , ("  "<>) .           show $ insertionEvents ac
          , ("  "<>) . concatMap show $ psuedoCharacter ac
          ]

readPseudoCharacter :: [Char] -> [PsuedoIndex]
readPseudoCharacter ('O':xs) = OriginalBase : readPseudoCharacter xs
readPseudoCharacter ('I':xs) = InsertedBase : readPseudoCharacter xs
readPseudoCharacter ('D':xs) = DeletedBase  : readPseudoCharacter xs
readPseudoCharacter ('X':xs) = DelInsBase   : readPseudoCharacter xs
readPseudoCharacter ('-':xs) = HardGap      : readPseudoCharacter xs
readPseudoCharacter ('~':xs) = SoftGap      : readPseudoCharacter xs
readPseudoCharacter       _  = []

--TODO: remove the actual character.

applyLocalEventsToAlignment :: (Eq c, Eq e, Show c, Show e) => e -> DeletionEvents -> InsertionEvents c e -> AlignmentContext c e -> AlignmentContext c e
--applyLocalEventsToAlignment _ _ x | trace ("\n\nInput:\n"<>show x) False = undefined
applyLocalEventsToAlignment edgeIdentifier localDeletionEvents localInsertionEvents alignmentContext --  (\x -> trace ("\nOutput\n"<>show x) x)
  | null errors = result
  | otherwise   = trace (intercalate "\n" errors) result 
  where
    edgeStr = show edgeIdentifier
    result  =
      Context
         { insertionEvents = IE.wrap resultInserts
         , psuedoCharacter = reverse resultChar
         }

    errors = catMaybes [badResMessage, badInsMessage, badDelMessage]

    badResMessage
      | consistency result = Nothing
      | otherwise = Just $ edgeStr <> " Inconsistent result: " <> show result

    badInsMessage
      | null leftoverLocalInsertions = Nothing
      | otherwise = Just $ edgeStr <> " Leftover insertions: " <> show (IE.wrap leftoverLocalInsertions)

    badDelMessage
      | onull leftoverDeletions = Nothing
      | otherwise = Just $ edgeStr <> " Leftover deletions: "  <> show (DE leftoverDeletions)

    (_,_,_,leftoverDeletions, leftoverLocalInsertions, _leftoverGlobalInsertions, resultInserts, resultChar) = foldl' f initalAccumulator $ psuedoCharacter alignmentContext

    initalAccumulator = (0, 0, 0 :: Int, (\(DE x) -> x) localDeletionEvents, IE.unwrap localInsertionEvents, IE.unwrap $ insertionEvents alignmentContext, mempty, mempty)

--    f q e | trace (show e <> show q) False = undefined
--    f _q@(pBasesSeen, cBasesSeen, consecutiveInsertionsSkipped,  remainingLocalInsertions, unappliedGlobalInsertions, is, es) e | trace (show remainingLocalInsertions) False = undefined
    f _q@(pBasesSeen, cBasesSeen, consecutiveInsertionsSkipped, remainingDeletions, remainingLocalInsertions, unappliedGlobalInsertions, is, cs) pc =
      case pc of
        -- In the case of an original base, we migth delete the base. if so we must decrement the resulting InsertionEvents. We place in a deleted base. This can probably be replaced with a HardGap.
        OriginalBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , 0                               , pBasesSeen `IS.delete` remainingDeletions, remainingLocalInsertions, unappliedGlobalInsertions, is, DeletedBase : cs)
                   else (pBasesSeen + 1, cBasesSeen + 1, 0                               ,                        remainingDeletions, remainingLocalInsertions, unappliedGlobalInsertions, is,          pc : cs)
        -- In the case of an original base, we migth delete the base. if so we must decrement the resulting InsertionEvents. We place in a HardGap.
        InsertedBase ->
                   if    pBasesSeen `oelem` localDeletionEvents
                   then (pBasesSeen + 1, cBasesSeen    , consecutiveInsertionsSkipped + 1, pBasesSeen `IS.delete` remainingDeletions, remainingLocalInsertions, unappliedGlobalInsertions, is, DelInsBase : cs)
                   else (pBasesSeen + 1, cBasesSeen + 1, consecutiveInsertionsSkipped + 1,                        remainingDeletions, remainingLocalInsertions, unappliedGlobalInsertions, is,         pc : cs)

        -- Same logic in nesxt two cases, essentially the identifunction of the accumulator.
        DeletedBase  -> (pBasesSeen    , cBasesSeen    , 0                               , remainingDeletions,       remainingLocalInsertions, unappliedGlobalInsertions, is,        pc : cs)
        DelInsBase   -> (pBasesSeen    , cBasesSeen    , 0                               , remainingDeletions,       remainingLocalInsertions, unappliedGlobalInsertions, is,        pc : cs)
--        HardGap      -> (pBasesSeen    , cBasesSeen    , 0                               remainingDeletions,        remainingLocalInsertions, unappliedGlobalInsertions, is,        e : es) -- maybe increment consecutive here too?
        HardGap      -> (pBasesSeen    , cBasesSeen    , 0                               , remainingDeletions,       remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.take 1 (unappliedGlobalInsertions ! pBasesSeen)) is,       pc : cs) -- maybe increment consecutive here too?
        -- The super complicated case... let nme try and explain.
        --
        -- First Check to see if the bases 
        SoftGap      -> conditionallyInsert
      where
        conditionallyInsert =
          case (pBasesSeen `lookup` unappliedGlobalInsertions) >>= (0 `lookup`) of
            Nothing    -> error ("Inconsistent indexing @ " <> show pBasesSeen <> ": " <> show remainingLocalInsertions <> show unappliedGlobalInsertions <> show (insertionEvents alignmentContext))
--                        (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1,                        remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, is,                     e : es)
            Just a@(c,e) ->
              if e /= edgeIdentifier
              then (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1, remainingDeletions,                       remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.singleton a) is,                    pc : cs)
              else
{--} -- Just sanity checking here!                
                case pBasesSeen `lookup` remainingLocalInsertions of
                  Nothing -> error $ "The base " <> show c <> " was in the local global event set for this edge, but not in the local insertion set." 
                  Just bs ->
                    let (b,_) = bs ! 0
                    in
                      if b /= c
                      then error $ "The base from the local insertionevents " <> show b <> " did not match the base from the globel events " <> show c
{--}
                      else (pBasesSeen, cBasesSeen + 1, 0                               , remainingDeletions, imRemove pBasesSeen 0 remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions,                                    is,          InsertedBase : cs)

{-              
              case pBasesSeen `lookup` remainingLocalInsertions of
                Nothing -> (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1, remainingDeletions,                        remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.singleton c) is,                     e : es)
                Just bs ->
                  let b = bs ! 0
                  in
                    if b /= c
                    then   (pBasesSeen, cBasesSeen    , consecutiveInsertionsSkipped + 1, remainingDeletions,                       remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions, imAdd cBasesSeen (Seq.singleton c) is,                     e : es)
                    else   (pBasesSeen, cBasesSeen + 1, 0                               , remainingDeletions, imRemove pBasesSeen 0 remainingLocalInsertions, imRemove pBasesSeen 0 unappliedGlobalInsertions,                                    is,          InsertedBase : es)
-}


imDec :: IS.Key -> IntMap a -> IntMap a
imDec i = IM.mapKeysMonotonic f
  where
    f k
      | k >= i    = k -1
      | otherwise = k


imAdd :: IS.Key -> Seq a -> IntMap (Seq a) -> IntMap (Seq a)
imAdd = IM.insertWith (flip (Seq.><) )

imInc :: IS.Key -> IntMap a -> IntMap a
imInc i = IM.mapKeysMonotonic f
  where
    f k
      | k >= i    = k + 1
      | otherwise = k

tail' :: [a] -> [a]
tail'    []  = []
tail' (_:xs) = xs

imRemove :: Int -> Int -> IntMap (Seq a) -> IntMap (Seq a)
imRemove k i = IM.update f k
  where
    f v
      | Seq.null v' = Nothing
      | otherwise   = Just v'
      where
        v' = as <> Seq.drop 1 bs
        (as, bs) = Seq.splitAt i v


consistency :: (Eq c, Eq e) => AlignmentContext c e -> Bool
consistency ac = truth
  where
    m = IE.unwrap $ insertionEvents ac
    (_,_,truth) = foldl f (0,0,True) $ psuedoCharacter ac 

    f a@(_,_,False) _ = a
    f (basesSeen, consecutiveBases, result) e = 
      case e of
        OriginalBase -> (basesSeen + 1, 0                   , result)
        InsertedBase -> (basesSeen + 1, 0                   , result)
        DeletedBase  -> (basesSeen    , 0                   , result)
        DelInsBase   -> (basesSeen    , 0                   , result)
        HardGap      -> (basesSeen    , consecutiveBases + 1, isJust ((basesSeen `lookup` m) >>= (consecutiveBases `lookup`)))
        SoftGap      -> (basesSeen    , consecutiveBases + 1, isJust ((basesSeen `lookup` m) >>= (consecutiveBases `lookup`)))
    
