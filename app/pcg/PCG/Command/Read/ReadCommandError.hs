{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PCG.Command.Read.ReadCommandError
  ( ReadCommandError(..)
  , ambiguous
  , unaligned
  , unfindable
  , unopenable
  , unparsable
  ) where

import Control.DeepSeq                   (NFData)
import GHC.Generics                      (Generic)
--import Data.Data
import Data.FileSource                   (FileSource)
import Data.List.NonEmpty                (NonEmpty (..))
import Data.Semigroup.Foldable
import Data.Unification
import PCG.Command.Read.InputStreamError
import PCG.Command.Read.ParseStreamError
import Text.Megaparsec
import TextShow


-- |
-- The various ways in which a 'Read' 'Command' from a POY script can fail.
-- A single 'Read' 'Command' can fail in multiple ways simultaneously.
-- To account for this the 'ReadCommandError' type is a composable 'Semigroup' to
-- allow for the collection of possible sub errors to be coalesced into a single
-- 'ReadCommandError' value. The `show` definition will render the 'ReadCommandError'
-- as a human legible collection of errors that occured within the 'Read' 'Command'.
--
-- Errors related to reading input streams from the @Real World@ are bundled
-- together as are errors from parsing input streams and errors from unifying the
-- data coherently together. Input stream errors will short-circuit any parse
-- errors, and parse errors will short circuit any unification errors.
--
-- The semigroup operator '(<>)' handles bundling and short circuiting.
data  ReadCommandError
    = InputError InputStreamError
    | ParseError ParseStreamError
    | UnifyError UnificationError
    deriving (Generic, NFData, Show)


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


ambiguous :: Foldable1 f => FileSource -> f FileSource -> ReadCommandError
ambiguous path = InputError . makeAmbiguousFiles path


unaligned :: Integral i => FileSource -> NonEmpty i -> ReadCommandError
unaligned path = ParseError . makeInvalidPrealigned path


unfindable :: FileSource -> ReadCommandError
unfindable = InputError . makeFileNotFound


unopenable :: FileSource -> ReadCommandError
unopenable = InputError . makeFileNotOpenable


unparsable :: (ShowErrorComponent e, Stream s) => FileSource -> ParseErrorBundle s e -> ReadCommandError
unparsable path = ParseError . makeUnparsableFile path
