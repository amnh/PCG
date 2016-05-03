----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the Henning or TNT file format.
-- Parses a useful subset of the file specification.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

-- TODO: Document and restrict Internal exports!
module File.Format.TNT
  ( CharacterMetaData(..)
  , LeafyTree(..)
  , TaxonInfo
  , TntCharacter(..)
  , TntResult
  , TRead
  , TReadTree
  , TreeOnly
  , NodeType(..)
  , WithTaxa(..)
  , TaxonName
  , TaxonSequence
  , TntContinuousCharacter
  , TntDiscreteCharacter
  , TntDnaCharacter
  , TntProteinCharacter
  , tntStreamParser
  ) where

import File.Format.TNT.Parser (tntStreamParser)
import File.Format.TNT.Internal
