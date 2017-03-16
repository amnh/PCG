-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Ranged
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Bio.Character.Encodable.Ranged
  ( Range()
  , Ranged(..)
  ) where

import           Bio.Character.Encodable.Internal
import           Bio.Character.Exportable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import           Data.BitVector     hiding (concat)
import           Data.Foldable
import           Data.List                 (intercalate)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.String               (IsString)
import           Foreign.C.Types


data Range r = Range (r, r)

type family Bound (f :: * -> *)

class Ranged a where

    toRange :: a -> Range (Bound a)

    fromRange :: Range (Bound a) -> a


