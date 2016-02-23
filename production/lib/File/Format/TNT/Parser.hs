{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

--import Data.Foldable 
import           Data.List.NonEmpty (NonEmpty,fromList, toList)
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
