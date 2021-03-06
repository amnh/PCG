------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Software.Metadata
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PCG.Software.Metadata
  ( softwareName
  , shortVersionInformation
  , fullVersionInformation
  ) where

import Data.List.NonEmpty                 (NonEmpty(..))
import Data.Semigroup.Foldable
import Data.String
import Data.Version                       (showVersion)
import Development.GitRev                 (gitCommitCount, gitHash)
import Paths_phylogenetic_component_graph (version)


-- |
-- Name of the software package.
softwareName :: IsString s => s
softwareName = "Phylogenetic Component Graph"


-- |
-- Brief description of the software version.
shortVersionInformation :: (IsString s, Semigroup s) => s
shortVersionInformation = "(alpha) version " <> fromString (showVersion version)


-- |
-- Full escription of the software version.
--
-- Uses @TemplateHaskell@ to splice in git hash and commit count information
-- from the compilation environment.
fullVersionInformation :: (IsString s, Semigroup s) => s
fullVersionInformation = fold1 $
    softwareName :|
    [ " "
    , shortVersionInformation
    , " ["
    , fromString $ take 7 $(gitHash)
    , "] ("
    , fromString $(gitCommitCount)
    , " commits)"
    ]
