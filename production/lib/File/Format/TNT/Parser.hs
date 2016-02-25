{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

import           Control.Monad            ((<=<),liftM3)
import           Data.Foldable            (toList)
import           Data.IntMap              (IntMap,insertWith)
import qualified Data.IntMap        as IM (lookup)
import qualified Data.Map           as M  (fromList,lookup)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Maybe               (fromMaybe)
import           Data.Vector              (Vector,(!),(//),generate)
import qualified Data.Vector        as V  (fromList)
-- import           Data.Vector (fromList)
--import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.CNames
--import           File.Format.TNT.Command.Procedure
--import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           File.Format.TNT.Partitioning
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Prim                     (MonadParsec)

tntStreamParser :: MonadParsec s m Char => m TntResult
tntStreamParser = (colateResult <=< collapseStructures) =<< (whitespace *> gatherCommands)
  where
    colateResult :: MonadParsec s m Char => ([CCode],[CharacterName],[TReadTree],[XRead]) -> m TntResult
    colateResult (     _,     _,     _,  x:y:z) = fail "Multiple XREAD commands found in source, expecting a single XREAD command."
    colateResult (     _,     _,    [],     []) = fail "No XREAD command or TREAD command, expecting either a single XREAD command or one or more TRead commands."
    colateResult (     _,     _,treads,     []) = pure . Left $ NE.fromList treads
    colateResult (ccodes,cnames,treads,[xread])
      | charCountx xread == 0 = (Left . fmap (fmap (Name . fst)) . NE.fromList) <$> matchTaxaInTree xread treads
      | otherwise             = fmap Right
                              $ liftM3 (WithTaxa)
                                  (pure $ vectorizeTaxa xread)
                                  (pure . applyCNames cnames $ ccodeCoalesce (taxaCountx xread) ccodes)
                                  (matchTaxaInTree xread treads)
      where
        vectorizeTaxa   = V.fromList . toList . sequencesx
        matchTaxaInTree :: MonadParsec s m Char => XRead -> [TReadTree] -> m [LeafyTree TaxonInfo]
        matchTaxaInTree xread = traverse interpolateLeafs
          where
            seqs  = sequencesx xread
            vseqs = V.fromList $ toList seqs
            mseqs = M.fromList $ toList seqs
            limit = length vseqs - 1
            interpolateLeafs :: MonadParsec s m Char => TReadTree -> m TNTTree
            interpolateLeafs = traverse substituteLeaf
            substituteLeaf (Index i)
              | 0 > i || i >= limit = fail $ "Index '" ++ show i ++ "' in TREAD tree is outside the range of taxa [0," ++ show limit ++ "]."
              | otherwise           = pure (vseqs ! i)
            substituteLeaf (Name name) =
              case name `M.lookup` mseqs of
                Nothing -> fail $ "Name '" ++ show name ++ "' in TREAD tree is not in the list of taxa from the XREAD commands."
                Just x  -> pure $ (name, x)
            substituteLeaf (Prefix pref) = undefined

collapseStructures :: MonadParsec s m Char => ([CCode],[CNames],[TRead],[XRead]) -> m ([CCode],[CharacterName],[TReadTree],[XRead])
collapseStructures (ccodes,cnames,treads,xreads)
  | not (null errors) = fails errors
  | otherwise         = pure (ccodes,collapsedCNames,collapsedTReads,xreads)
  where
    errors          = cnamesErrors 
    collapsedTReads = concatMap toList treads 
    collapsedCNames = concatMap toList cnames
    cnamesErrors    = if null collapsedCNames
                      then []
                      else duplicateIndexMessages $ NE.fromList collapsedCNames

applyCNames :: Foldable f => f CharacterName -> Vector CharacterMetaData -> Vector CharacterMetaData
applyCNames charNames metaData = metaData // (foldl f [] charNames)
  where
    f :: [(Int, CharacterMetaData)] -> CharacterName -> [(Int, CharacterMetaData)]
    f xs name = (i, modifyMetaDataNames name (metaData ! i)) : xs
      where
        i = sequenceIndex name
    
-- | Coalesces many CCODE commands respecting thier structural order
--   into a single index ordered mapping.
ccodeCoalesce :: Foldable t => Int -> t CCode -> Vector CharacterMetaData
ccodeCoalesce charCount ccodeCommands = generate charCount f
  where
    f :: Int -> CharacterMetaData
    f = fromMaybe initialMetaData . (`IM.lookup` stateMapping)
    stateMapping :: IntMap CharacterMetaData
    stateMapping = foldl addChangeSet mempty ccodeCommands
    addChangeSet :: IntMap CharacterMetaData -> CCode -> IntMap CharacterMetaData
    addChangeSet mapping (CCode state indicies) = foldl applyChanges mapping indicies
      where
        applyChanges :: IntMap CharacterMetaData -> CharacterSet -> IntMap CharacterMetaData
        applyChanges mapping' changeSet = foldl (insertState state) mapping' range
          where
            range = case changeSet of
                     Single    i   -> [i..i]
                     Range     i j -> [i..j]
                     FromStart   j -> [0..j]
                     ToEnd     i   -> [i..charCount]
                     Whole         -> [0..charCount]
    insertState :: CharacterState -> IntMap CharacterMetaData ->  Int -> IntMap CharacterMetaData
    insertState state mapping index = insertWith translation index defaultValue mapping
      where
        defaultValue = metaDataTemplate state
        translation  = const (modifyMetaDataState state)

