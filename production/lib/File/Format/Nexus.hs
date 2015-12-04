{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus
  ( CharDataType(..)
  , Nexus(..)
  , nexusStreamParser
  ) where

import File.Format.Nexus.Parser
import Text.Megaparsec
import Text.Megaparsec.Prim      (MonadParsec)

nexusStreamParser :: (Show s, MonadParsec s m Char) => m Nexus
nexusStreamParser = validateParseResult =<< parseNexus <* eof
