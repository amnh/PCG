{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

--import Data.Foldable 
import           Data.IntMap              (IntMap,insertWith)
import qualified Data.IntMap        as IM (lookup)
import           Data.List.NonEmpty       (NonEmpty,fromList, toList)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Maybe               (fromMaybe)
import           Data.Vector              (Vector,generate)
-- import           Data.Vector (fromList)
import           File.Format.TNT.Command.CCode
--import           File.Format.TNT.Command.Procedure
--import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           File.Format.TNT.Partitioning
import           Text.Megaparsec.Prim                     (MonadParsec)

-- TODO: make the types better
tntStreamParser :: MonadParsec s m Char => m TntResult
tntStreamParser = colateResult  =<< (whitespace *> gatherCommands)
  where
    colateResult (     _,     _,  x:y:z) = fail "Multiple XREAD commands found in source, expecting a single XREAD command."
    colateResult (     _,    [],     []) = fail "No XREAD command or TRead command, expecting either a single XREAD command or one or more TRead commands."
    colateResult (     _,treads,     []) = pure . Left $ concatTReads treads
    colateResult (ccodes,treads,[xread])
      | charCountx xread == 0 = pure . Left . fmap (fmap (Name . fst)) . fromList . matchTaxaInTree xread $ concatTReads treads
--      | otherwise             = pure . Right . WithTaxa xread (ccodeCoalesce ccodes) (matchTaxaInTree xread $ concatTreads treads)
      where
        matchTaxaInTree :: XRead -> TRead -> [LeafyTree TaxonInfo]
        matchTaxaInTree = undefined

concatTReads :: Foldable f => f TRead -> TRead
concatTReads = fromList . concatMap toList

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
