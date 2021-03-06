-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.ReadCommandError
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Composable error type representing one or more failures in the READ command.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Command.Read.ReadCommandError
  ( ReadCommandError(..)
  , ambiguous
  , unaligned
  , unparsable
  ) where


import Control.DeepSeq                  (NFData)
import Data.Data
import Data.FileSource                  (FileSource)
import Data.FileSource.InputStreamError
import Data.FileSource.ParseStreamError
import Data.List.NonEmpty               (NonEmpty(..))
import Data.Semigroup.Foldable
import Data.Unification
import GHC.Generics                     (Generic)
import Text.Megaparsec
import TextShow


-- |
-- The various ways in which a 'Read' 'PCG.Syntax.Parser.Command' from a POY
-- script can fail. A single 'Read' 'PCG.Syntax.Parser.Command' can fail in
-- multiple ways simultaneously. To account for this the 'ReadCommandError' type
-- is a composable 'Semigroup' to allow for the collection of possible sub errors
-- to be coalesced into a single 'ReadCommandError' value. The `show` definition
-- will render the 'ReadCommandError' as a human legible collection of errors
-- that occurred within the 'Read' 'PCG.Syntax.Parser.Command'.
--
-- Errors related to reading input streams from the @Real World@ are bundled
-- together as are errors from parsing input streams and errors from unifying the
-- data coherently together. Input stream errors will short-circuit any parse
-- errors, and parse errors will short circuit any unification errors.
--
-- The semigroup operator '(<>)' handles bundling and short circuiting.
data  ReadCommandError
    = InputError {-# UNPACK #-} !InputStreamError
    | ParseError {-# UNPACK #-} !ParseStreamError
    | UnifyError {-# UNPACK #-} !UnificationError
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


instance Semigroup ReadCommandError where

    lhs <> rhs =
        case (lhs, rhs) of
          (InputError xs, InputError ys) -> InputError $ xs <> ys
          (InputError  _,             _) -> lhs
          (ParseError xs, ParseError ys) -> ParseError $ xs <> ys
          (ParseError  _, InputError  _) -> rhs
          (ParseError  _, UnifyError  _) -> lhs
          (UnifyError xs, UnifyError ys) -> UnifyError $ xs <> ys
          (UnifyError  _,             _) -> rhs


instance TextShow ReadCommandError where

    showb (InputError v) = showb v
    showb (ParseError v) = showb v
    showb (UnifyError v) = showb v


-- |
-- Create a 'ReadCommandError' representing an ambiguous file source when an
-- unambiguous file source was expected.
ambiguous :: Foldable1 f => FileSource -> f FileSource -> ReadCommandError
ambiguous path = InputError . makeAmbiguousFiles path


-- |
-- Create a 'ReadCommandError' representing unaligned file content when a file
-- source was marks as having pre-aligned content.
unaligned :: Integral i => FileSource -> NonEmpty i -> ReadCommandError
unaligned path = ParseError . makeInvalidPrealigned path


-- |
-- Create a 'ReadCommandError' representing that a file's contents could not be
-- parsed.
unparsable
  :: ( ShowErrorComponent e
     , TraversableStream s
     , VisualStream s
     )
  => FileSource
  -> ParseErrorBundle s e
  -> ReadCommandError
unparsable path = ParseError . makeUnparsableFile path
