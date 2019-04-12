{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module PCG.Command.Read.ParseStreamError
  ( ParseStreamError()
  , makeInvalidPrealigned
  , makeUnparsableFile
  ) where

import Control.Arrow
import Control.DeepSeq           (NFData)
--import Data.Data
import Data.FileSource
import Data.Foldable
import Data.List.NonEmpty hiding (toList)
import GHC.Generics              (Generic)
import Text.Megaparsec
import TextShow
import TextShow.Custom


-- |
-- The various ways in which a 'Read' 'Command' from a POY script can fail.
-- A single 'Read' 'Command' can fail in multiple ways simultaneously.
-- To account for this the 'InputStreamError' type is a composable 'Semigroup' to allow
-- for the collection of possible sub errors to be coalesced into a single
-- 'InputStreamError' value. The `show` definition will render the 'Read Error' as a
-- human legible collection of errors that occured within the 'Read' 'Command'.
newtype ParseStreamError = ParseStreamError (NonEmpty ParseStreamErrorMessage)
    deriving (Generic, NFData, Show)


data  ParseStreamErrorMessage
    = FileUnparsable    FileSource String
    | InvalidPrealigned FileSource (NonEmpty Word)
    deriving (Generic, NFData, Show)
   

instance Semigroup ParseStreamError where

    (ParseStreamError lhs) <> (ParseStreamError rhs) = ParseStreamError $ lhs <> rhs


instance TextShow ParseStreamError where

    showb (ParseStreamError errors) = uncurry (<>) . (showUnparsable *** showUnaligned) $ partitionParseStreamErrors errors
      where
        showUnparsable pErrors = unlinesB [preamble, errorMessages]
          where
            preamble =
                case fst <$> pErrors of
                  []   -> ""
                  [x]  -> "Could not parse file " <> showb x <> "\n"
                  x:xs -> unlinesB $ showb <$> ("Could not parse the following files:":x:xs)

            errorMessages = unlinesB $ showb . snd <$> pErrors

        showUnaligned aErrors =
            case aErrors of
              []  -> ""
              [x] -> unlinesB   [ "The file was specified as prealigned," , "but not all characters had the same length.", showInvalidPrealigned x ]
              xs  -> unlinesB $ [ "The following files were specified as prealigned,", "but not all characters had the same length:"] <> (showInvalidPrealigned <$> xs)
          where
            showInvalidPrealigned :: (FileSource, NonEmpty Word) -> Builder
            showInvalidPrealigned (path, cols) = fold ["'", showb path, "', has characters of lengths ", intercalateB ", " $ showb <$> cols]

        partitionParseStreamErrors = foldr f ([],[])
          where
            f e (x,y) =
              case e of
                FileUnparsable    a b -> ((a,b):x,       y)
                InvalidPrealigned a b -> (      x, (a,b):y)


-- |
-- Remark that a parsing error occured when reading the file. Note that the 'ParseError' should contain the 'FileSource' information.
makeUnparsableFile :: (ShowErrorComponent e, Stream s) => FileSource -> ParseErrorBundle s e -> ParseStreamError
makeUnparsableFile path pErr = ParseStreamError . pure . FileUnparsable path $ errorBundlePretty pErr


makeInvalidPrealigned :: Integral i => FileSource -> NonEmpty i -> ParseStreamError
makeInvalidPrealigned path = ParseStreamError . pure . InvalidPrealigned path . fmap (fromIntegral . abs)
