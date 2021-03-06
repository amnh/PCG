------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.General
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Bio.Metadata.General
  ( GeneralCharacterMetadataDec()
  , GeneralCharacterMetadata(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , generalMetadata
  ) where

import Bio.Metadata.General.Class
import Bio.Metadata.General.Internal
