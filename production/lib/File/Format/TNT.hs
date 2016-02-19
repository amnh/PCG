{-# LANGUAGE FlexibleContexts #-}

module File.Format.TNT
  ( tntStreamParser
  ) where

import File.Format.TNT.Internal
--import File.Format.TNT.Command.Partition
import Text.Megaparsec.Prim     (MonadParsec)

tntStreamParser :: MonadParsec s m Char => m TntResult
tntStreamParser = undefined


