-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fastc
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing FASTC files into a naive sequence form.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fastc
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , fastcStreamParser
  ) where

import File.Format.Fastc.Parser
