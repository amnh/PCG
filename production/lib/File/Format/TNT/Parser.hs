{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

import           Data.List.NonEmpty (toList)
import           Data.Vector (fromList)
import           File.Format.TNT.Command.CCode
--import           File.Format.TNT.Command.Procedure
--import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           File.Format.TNT.Partitioning
import           Text.Megaparsec.Prim                     (MonadParsec)

-- TODO: make the types better
tntStreamParser :: MonadParsec s m Char => m TntResult
tntStreamParser = do
    _                 <- whitespace
    (xreads,ccodes,_) <- gatherCommands
    xread             <- singleXRead xreads
    pure . Right $ WithTaxa (fromList . toList $ sequencesx xread) (ccodeCoalesce (charCountx xread) ccodes) mempty
  
singleXRead :: MonadParsec s m Char => [XRead] -> m XRead
singleXRead xreads
  | null xreads              = fail "No XREAD command found in source."
  | (not.isSingleton) xreads = fail "Multiple XREAD commands found in source, expecting a single XREAD command."
  | otherwise                = pure $ head xreads 

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
