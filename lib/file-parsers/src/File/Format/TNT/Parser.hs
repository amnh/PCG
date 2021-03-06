----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Parser
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the entire TNT file which returns a colated result of TNT
-- taxa and their possible corresponding character sequences along with a
-- possible forest of trees defined for the taxa set.
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.TNT.Parser
  ( tntStreamParser
  ) where

import           Control.Monad                  (liftM3, (<=<))
import           Data.CaseInsensitive           (FoldCase)
import           Data.Foldable
import           Data.IntMap                    (IntMap, insertWith, mapWithKey, toAscList)
import qualified Data.IntMap                    as IM (lookup)
import qualified Data.List.NonEmpty             as NE (fromList)
import qualified Data.Map                       as M (fromList, lookup)
import           Data.Matrix.NotStupid          (Matrix)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Data.Vector                    (Vector, generate, (!), (//))
import qualified Data.Vector                    as V (fromList)
import           Data.Void
import           File.Format.TNT.Command.CNames
import           File.Format.TNT.Internal
import           File.Format.TNT.Partitioning
import           Text.Megaparsec
import           Text.Megaparsec.Custom


-- |
-- Parses the contents of a TNT file stream into a 'TntResult'. A file stream
-- can contain either:
--
-- * Only a forest of trees with labeled leaf nodes and the set of leaf nodes
--   is the same across the forest.
--
-- * A collection of taxa sequences with coresponsing metadata and possibly
--   corresponding forest of trees whose leaf sets are equal to the taxa set.
{-# SPECIALISE tntStreamParser :: Parsec Void  T.Text TntResult #-}
{-# SPECIALISE tntStreamParser :: Parsec Void LT.Text TntResult #-}
{-# SPECIALISE tntStreamParser :: Parsec Void  String TntResult #-}
tntStreamParser :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m TntResult
tntStreamParser = (colateResult <=< collapseStructures) =<< (whitespace *> gatherCommands)
  where
    colateResult :: MonadFail m => ([CCode],[CharacterName],[Cost],[NStates],[TReadTree],[XRead]) -> m TntResult
    colateResult (     _,     _,     _,      _,     _,  _:_:_) = fail "Multiple XREAD commands found in source, expecting a single XREAD command."
    colateResult (     _,     _,     _,      _,    [],     []) = fail "No XREAD command or TREAD command, expecting either a single XREAD command or one or more TRead commands."
    colateResult (     _,     _,    _,       _,treads,     []) = pure . Left $ NE.fromList treads
    colateResult (ccodes,cnames,costs,_nstates,treads,[xread])
      | charCountx xread == 0 = Left . fmap (fmap (Name . fst)) . NE.fromList <$> matchTaxaInTree xread treads
      | otherwise             = Right <$> liftM3 WithTaxa
                                  (pure $ vectorizeTaxa xread)
                                  (pure . applyCosts costs . applyCNames cnames $ ccodeCoalesce (charCountx xread) ccodes)
                                  (matchTaxaInTree xread treads)
      where
        vectorizeTaxa   = V.fromList . toList . sequencesx

        matchTaxaInTree :: MonadFail m => XRead -> [TReadTree] -> m [LeafyTree TaxonInfo]
        matchTaxaInTree xreadCommand = traverse interpolateLeafs
          where
            seqs  = sequencesx xreadCommand
            vseqs = V.fromList $ toList seqs
            mseqs = M.fromList $ toList seqs
            limit = length vseqs - 1
            interpolateLeafs = traverse substituteLeaf
            substituteLeaf (Index i)
              | 0 > i || i > limit = fail $ "Index '" <> show i <> "' in TREAD tree is outside the range of taxa [0," <> show limit <> "]."
              | otherwise          = pure (vseqs ! i)
            substituteLeaf (Name name) =
              case name `M.lookup` mseqs of
                Nothing -> fail $ "Name '" <> show name <> "' in TREAD tree is not in the list of taxa from the XREAD commands."
                Just x  -> pure (name, x)
            substituteLeaf (Prefix _) = undefined


-- |
-- Performs an initial structural collapse to the various type lists to make
-- subsequent folding easier.
collapseStructures :: (MonadParsec e s m {- , Token s ~ Char -}) => Commands -> m ([CCode],[CharacterName],[Cost],[NStates],[TReadTree],[XRead])
collapseStructures (ccodes, cnames, costs, nstates, treads, xreads)
  | not (null errors) = fails errors
  | otherwise         = pure (ccodes, collapsedCNames, costs, nstates, collapsedTReads, xreads)
  where
    errors          = cnamesErrors
    collapsedCNames = concatMap toList cnames
    collapsedTReads = concatMap toList treads
    cnamesErrors    = if null collapsedCNames
                      then []
                      else duplicateIndexMessages $ NE.fromList collapsedCNames


-- |
-- Mutate the metadata structure by replacing the default character naming
-- information with information defined in CNAME command(s).
applyCNames :: Foldable f => f CharacterName -> Vector CharacterMetadata -> Vector CharacterMetadata
applyCNames charNames metaData = metaData // toAscList names
  where
    names = f `mapWithKey` cnamesCoalesce charNames
    f i name = modifyMetaDataNames name $ metaData ! i

    cnamesCoalesce :: Foldable f => f CharacterName -> IntMap CharacterName
    cnamesCoalesce = foldl g mempty
      where
        g mapping name = insertWith const (sequenceIndex name) name mapping


-- |
-- Mutate the metadata structure by replacing the default TCM costs with
-- custom TCMs defined in COST command(s).
applyCosts :: Foldable f => f Cost -> Vector CharacterMetadata -> Vector CharacterMetadata
applyCosts charCosts metaData = metaData // toAscList matricies
  where
    matricies = mapWithKey f $ costsCoalesce (length metaData - 1) charCosts

    f i mat = modifyMetaDataTCM mat $ metaData ! i

    costsCoalesce :: Foldable f => Int -> f Cost -> IntMap (Matrix Double)
    costsCoalesce charCount = foldl addTCMs mempty
      where
        addTCMs mapping (Cost changeSet mat) = foldl (insertTCM mat) mapping $ range charCount changeSet
        insertTCM mat mapping index = insertWith const index mat mapping


-- |
-- Coalesces many CCODE commands respecting their structural order
-- into a single index ordered mapping.
ccodeCoalesce :: Foldable t => Int -> t CCode -> Vector CharacterMetadata
ccodeCoalesce charCount ccodeCommands = generate charCount f
  where
    f :: Int -> CharacterMetadata
    f = fromMaybe initialMetaData . (`IM.lookup` stateMapping)

    stateMapping :: IntMap CharacterMetadata
    stateMapping = foldl' (foldl' addChangeSet) mempty ccodeCommands

    addChangeSet :: IntMap CharacterMetadata -> CCodeAugment -> IntMap CharacterMetadata
    addChangeSet mapping (CCodeAugment states indices) = foldl' applyChanges mapping indices
      where
        applyChanges :: IntMap CharacterMetadata -> CharacterSet -> IntMap CharacterMetadata
        applyChanges mapping' changeSet = foldl' (insertStates states) mapping' (range charCount changeSet)

    insertStates :: Foldable t => t CharacterState -> IntMap CharacterMetadata ->  Int -> IntMap CharacterMetadata
    insertStates states mapping index = foldl' insertState mapping states
      where
        insertState mapping' state = insertWith translation index defaultValue mapping'
          where
            defaultValue = metaDataTemplate state
            translation  = const (modifyMetaDataState state)


-- |
-- Derive an ascending sequence of indices from a specified index range.
range :: Int -> CharacterSet -> [Int]
range _ (Single    i  ) = [i..i]
range _ (Range     i j) = [i..j]
range _ (FromStart   j) = [0..j]
range j (ToEnd     i  ) = [i..j]
range j  Whole          = [0..j]


