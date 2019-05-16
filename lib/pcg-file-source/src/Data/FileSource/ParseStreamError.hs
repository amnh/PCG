-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource.ParseStreamError
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Composable error type representing failure to parse an input stream.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnboxedSums        #-}

module Data.FileSource.ParseStreamError
  ( ParseStreamError()
  , makeInvalidPrealigned
  , makeDeserializeErrorInBinaryEncoding
  , makeDeserializeErrorInCompactRegion
  , makeUnparsableFile
  ) where

import Control.DeepSeq    (NFData)
--import Data.Data
import Data.FileSource
import Data.Foldable
import Data.List          (sortBy)
import Data.List.NonEmpty hiding (sortBy, toList)
import Data.Maybe         (catMaybes)
import Data.Ord           (comparing)
import Data.String
import Data.Text          (Text)
import Data.Text.Short    (ShortText, toShortByteString)
import GHC.Generics       (Generic)
import Text.Megaparsec
import TextShow           hiding (fromString)
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
    = FileUnparsable     {-# UNPACK #-} !FileSource {-# UNPACK #-} ! Text
    | InvalidPrealigned  {-# UNPACK #-} !FileSource {-# UNPACK #-} !(NonEmpty Word)
    | FileBadDeserialize {-# UNPACK #-} !FileSource !DataSerializationFormat {-# UNPACK #-} !ShortText
    deriving (Generic, NFData, Show)


data  DataSerializationFormat
    = BinaryFormat
    | CompactFormat
    deriving (Generic, NFData, Show)


instance Semigroup ParseStreamError where

    (ParseStreamError lhs) <> (ParseStreamError rhs) = ParseStreamError $ lhs <> rhs


instance TextShow ParseStreamError where

    showb (ParseStreamError errors) = unlinesB $ catMaybes
        [ showUnparsable
        , showUnaligned
        , showDeserializationFailures
        ]
      where
        (pErrors, aErrors, dErrors) = partitionParseStreamErrors errors

        showUnparsable =
            case sortBy (comparing fst) pErrors of
              []   -> Nothing
              x:xs -> Just $ unlinesB
                  [ preamble      $ fst <$> (x:|xs)
                  , errorMessages $ snd <$> (x:|xs)
                  ]
          where
            preamble (x:|xs) =
                case xs of
                  [] -> "Could not parse file '" <> showb x <> "'\n"
                  _  -> unlinesB . ("Could not parse the following files:":) $ showbIndent xs

            errorMessages = intercalateB "\n" . fmap fromText

        showUnaligned =
            case aErrors of
              []  -> Nothing
              [x] -> Just $ unlinesB   [ "The file was specified as prealigned," , "but not all characters had the same length.", showInvalidPrealigned x ]
              xs  -> Just . unlinesB $ [ "The following files were specified as prealigned,", "but not all characters had the same length:"] <> (showInvalidPrealigned <$> xs)
          where
            showInvalidPrealigned :: (FileSource, NonEmpty Word) -> Builder
            showInvalidPrealigned (path, cols) = fold ["  ", showb path, ", has characters of lengths ", intercalateB ", " $ showb <$> cols]

        showDeserializationFailures =
            case dErrors of
              [] -> Nothing
              xs -> Just . unlinesB $ showBadDeserialize <$> xs

        partitionParseStreamErrors = foldr f ([],[],[])
          where
            f e (x,y,z) =
              case e of
                FileUnparsable     a b   -> ((a,b):x,       y,         z)
                InvalidPrealigned  a b   -> (      x, (a,b):y,         z)
                FileBadDeserialize a b c -> (      x,       y, (a,b,c):z)


-- |
-- Remark that a parsing error occured when reading the file. Note that the 'ParseError' should contain the 'FileSource' information.
makeUnparsableFile
  :: ( ShowErrorComponent e
     , Stream s
     )
  => FileSource
  -> ParseErrorBundle s e
  -> ParseStreamError
makeUnparsableFile path =
    ParseStreamError . pure . FileUnparsable path . fromString . errorBundlePretty


-- |
-- Remark that the file was marked as prealigned but the data within was not aligned.
makeInvalidPrealigned :: Integral i => FileSource -> NonEmpty i -> ParseStreamError
makeInvalidPrealigned path =
    ParseStreamError . pure . InvalidPrealigned path . fmap (fromIntegral . abs)


-- |
-- Remark that the file has could not be deserialized.
makeDeserializeErrorInBinaryEncoding :: FileSource -> ShortText -> ParseStreamError
makeDeserializeErrorInBinaryEncoding path =
    ParseStreamError . pure . FileBadDeserialize path BinaryFormat


-- |
-- Remark that the file has could not be deserialized.
makeDeserializeErrorInCompactRegion :: FileSource -> ShortText -> ParseStreamError
makeDeserializeErrorInCompactRegion path =
    ParseStreamError . pure . FileBadDeserialize path CompactFormat


showBadDeserialize :: (TextShow a, Show b) => (a, b, ShortText) -> Builder
showBadDeserialize (path, format, msg) =
    "'" <> showb path <> "' [" <> fromString (show format) <> "]: " <> showb (toShortByteString msg)


showbIndent :: (Functor f, TextShow s) => f s -> f Builder
showbIndent = fmap (\x -> showbSpace <> showbSpace <> showb x)
