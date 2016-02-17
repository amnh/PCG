{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           File.Format.TNT.Command.CCode
--import           File.Format.TNT.Command.Procedure
--import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           File.Format.TNT.Partitioning
import           Text.Megaparsec.Prim                     (MonadParsec)

-- TODO: make the types better
tntStreamParser :: MonadParsec s m Char => m Hennig
tntStreamParser = do
    (xreads,ccodes,_) <- gatherCommands
    xread             <- singleXRead xreads
    pure . Hennig (taxaCountx xread) (sequencesx xread) $ ccodeCoalesce (charCountx xread) ccodes
    
singleXRead :: MonadParsec s m Char => [XRead] -> m XRead
singleXRead xreads
  | null xreads              = error "No XREAD command found in source."
  | (not.isSingleton) xreads = error "Multiple XREAD commands found in source, expecting a single XREAD command."
  | otherwise                = pure $ head xreads 

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
