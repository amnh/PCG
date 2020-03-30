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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnboxedSums        #-}

module Data.FileSource.ParseStreamError
  ( ParseStreamError()
  , makeInvalidPrealigned
--  , makeDeserializeErrorInBinaryEncoding
  , makeDeserializeErrorInCompactRegion
  , makeUnparsableFile
  ) where

import           Control.DeepSeq         (NFData)
import           Data.Data
import           Data.FileSource
import           Data.Foldable
import           Data.List               (sortOn)
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Maybe              (catMaybes)
import           Data.Semigroup.Foldable
import           Data.String
import           Data.Text               (Text)
import           Data.Text.Short         (ShortText, toShortByteString)
import           Data.Vector.Unboxed     (Vector, fromList)
import qualified Data.Vector.Unboxed     as V
import           GHC.Generics            (Generic)
import           Text.Megaparsec
import           TextShow                hiding (fromString)
import           TextShow.Custom


-- |
-- The various ways in which iterpreting data read into PCG can fail.
--
-- Input streams can fail at interpreting the data in multiple ways. Only after
-- all input streams were successfully reterived, will interpretation of the
-- streams take place. All errors in interpreting input stream data should be
-- reported together.
--
-- To account for this the 'ParseStreamError' type is a composable 'Semigroup' to
-- allow for the collection of many possible interpretation errors to be
-- coalesced into a single 'ParseStreamError' value.
--
-- The 'TextShow' instance should be used to render the 'ParseStreamError' as a
-- human legible collection of "parse" errors that occurred while
-- attempting to interpretation data read into PCG.
--
-- The 'Show' instance should only be used for debugging purposes.
newtype ParseStreamError = ParseStreamError (NonEmpty ParseStreamErrorMessage)
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


data  ParseStreamErrorMessage
    = FileUnparsable     {-# UNPACK #-} !FileSource {-# UNPACK #-} !Text
    | InvalidPrealigned  {-# UNPACK #-} !FileSource {-# UNPACK #-} !(Vector Word)
    | FileBadDeserialize {-# UNPACK #-} !FileSource !DataSerializationFormat {-# UNPACK #-} !ShortText
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


data  DataSerializationFormat
    = BinaryFormat
    | CompactFormat
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


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
            case sortOn fst pErrors of
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
            showInvalidPrealigned :: (FileSource, Vector Word) -> Builder
            showInvalidPrealigned (path, cols) = fold ["  ", showb path, ", has characters of lengths ", intercalateB ", " $ showb <$> V.toList cols]

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
-- Remark that a parsing error occurred when reading the file. Note that the 'ParseError' should contain the 'FileSource' information.
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
makeInvalidPrealigned :: (Foldable1 f, Integral i) => FileSource -> f i -> ParseStreamError
makeInvalidPrealigned path =
    ParseStreamError . pure . InvalidPrealigned path . fromList . fmap (fromIntegral . abs) . toList . toNonEmpty


{-
-- |
-- Remark that the file has could not be deserialized.
makeDeserializeErrorInBinaryEncoding :: FileSource -> ShortText -> ParseStreamError
makeDeserializeErrorInBinaryEncoding path =
    ParseStreamError . pure . FileBadDeserialize path BinaryFormat
-}


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
