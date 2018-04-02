-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Continuous.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Encodable.Continuous.Internal where

import Bio.Character.Encodable.Continuous.Class
import Bio.Character.Encodable.Internal
import Control.Arrow ((&&&))
import Control.DeepSeq
import Data.Range
import GHC.Generics
import Numeric.Extended.Real
import Text.XML.Class

import System.Environment  -- for command-line arguments.
import System.Exit


main = getArgs >>= parse >>= putStr . tac

    tac  = unlines . reverse . lines

    parse []     = getContents
    parse fs     = concat `fmap` mapM readFile fs

    usage   = putStrLn "Usage: tac [-vh] [file ..]"
    version = putStrLn "Haskell tac 0.1"
    exit    = exitWith ExitSuccess
    die     = exitWith (ExitFailure 1)
