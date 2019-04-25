-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource.InputStreamError
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes several useful disk utility related functionality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.FileSource.InputStreamError
  ( InputStreamError()
  , makeAmbiguousFiles
  , makeEmptyFileStream
  , makeFileInUseOnRead
  , makeFileNoReadPermissions
  , makeFileNotFound
  ) where

import Control.DeepSeq         (NFData)
--import           Data.Coerce               (Coercible, coerce)
--import           Data.Data                 (Data)
import Data.FileSource
import Data.Foldable
import Data.List.NonEmpty      hiding (toList)
import Data.Maybe              (catMaybes)
import Data.Semigroup.Foldable
import Data.Text.Short         (ShortText, toShortByteString)
import GHC.Generics            (Generic)
import TextShow


-- |
-- The various ways in which a 'Read' 'Command' from a POY script can fail.
-- A single 'Read' 'Command' can fail in multiple ways simultaneously.
-- To account for this the 'InputStreamError' type is a composable 'Semigroup' to allow
-- for the collection of possible sub errors to be coalesced into a single
-- 'InputStreamError' value. The `show` definition will render the 'Read Error' as a
-- human legible collection of errors that occured within the 'Read' 'Command'.
newtype InputStreamError = InputStreamError (NonEmpty InputStreamErrorMessage)
    deriving (Generic, NFData, Show)


data  InputStreamErrorMessage
    = FileAlreadyInUse   FileSource
    | FileAmbiguous      FileSource (NonEmpty FileSource)
    | FileBadDeserialize FileSource DataSerializationFormat ShortText
    | FileBadPermissions FileSource
    | FileEmptyStream    FileSource
    | FileUnfindable     FileSource
    deriving (Generic, NFData, Show)


data  DataSerializationFormat
    = BinaryFormat
    | CompactFormat
    deriving (Generic, NFData, Show)


instance Semigroup InputStreamError where

    (InputStreamError lhs) <> (InputStreamError rhs) = InputStreamError $ lhs <> rhs


instance TextShow InputStreamError where

    showb (InputStreamError errors) = unlinesB $ catMaybes
        [ unfindableMessage
        , ambiguousMessage
        , unopenableMessage
        , alreadInUseMessage
        , emptyStreamsMessage
        , deserializationFailureMessage
        ]
      where
        (inUseFiles, ambiguity, deserialize, badPermissions, emptyStreams, unfindables) = partitionInputStreamErrorMessages $ toList errors

        alreadInUseMessage =
          case inUseFiles of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " is already in use"
            xs  -> Just $ "The following files were already in use:\n" <> unlinesB (showb <$> xs)

        ambiguousMessage =
          case ambiguity of
            [] -> Nothing
            xs -> Just . unlinesB $ showb <$> xs

        deserializationFailureMessage =
          case deserialize of
            [] -> Nothing
            xs -> Just . unlinesB $ showb <$> xs

        emptyStreamsMessage =
          case emptyStreams of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " was empty"
            xs  -> Just $ "The following files were empty:\n" <> unlinesB (showb <$> xs)

        unfindableMessage =
          case unfindables of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " does not exist"
            xs  -> Just $ "The following files do not exists:\n" <> unlinesB (showb <$> xs)

        unopenableMessage =
          case badPermissions of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " had premissions which prevent it from being opened"
            xs  -> Just $ "The following files have permissions which prevent them from being opened:\n" <> unlinesB (showb <$> xs)

        partitionInputStreamErrorMessages
          :: [InputStreamErrorMessage]
          -> ( [InputStreamErrorMessage]
             , [InputStreamErrorMessage]
             , [InputStreamErrorMessage]
             , [InputStreamErrorMessage]
             , [InputStreamErrorMessage]
             , [InputStreamErrorMessage]
             )
        partitionInputStreamErrorMessages = foldr f ([],[],[],[],[],[])
          where
            f e@FileAlreadyInUse   {} (u,v,w,x,y,z) = (e:u,   v,   w,   x,   y,   z)
            f e@FileAmbiguous      {} (u,v,w,x,y,z) = (  u, e:v,   w,   x,   y,   z)
            f e@FileBadDeserialize {} (u,v,w,x,y,z) = (  u,   v, e:w,   x,   y,   z)
            f e@FileBadPermissions {} (u,v,w,x,y,z) = (  u,   v,   w, e:x,   y,   z)
            f e@FileEmptyStream    {} (u,v,w,x,y,z) = (  u,   v,   w,   x, e:y,   z)
            f e@FileUnfindable     {} (u,v,w,x,y,z) = (  u,   v,   w,   x,   y, e:z)


instance TextShow InputStreamErrorMessage where

    showb (FileEmptyStream    path        ) = "'" <> showb path <> "'"
    showb (FileUnfindable     path        ) = "'" <> showb path <> "'"
    showb (FileBadPermissions path        ) = "'" <> showb path <> "'"
    showb (FileBadDeserialize path format msg) = "'" <> showb path <> "' [" <> fromString (show format) <> "]: " <> showb (toShortByteString msg)
    showb (FileAmbiguous      path matches) = message
      where
        files   = toList matches
        message = unlinesB
          [ "The following file specification is ambiguous:"
          , "\t'" <> showb path <> "'"
          , "The file specification should match a single file, but multiple matches were found:"
          , unlinesB $ (\x -> "'" <> showb x <> "'") <$> files
          ]
-- "Failed to deserialize compact region with error: \n"

-- |
-- Remark that the specified file path matches many possible files.
-- This should be used when a single file is expected but 'regex matching' or 'file globbing'
-- are present in the processing of the Read Command.
--
-- @ambiguous path matches@ notes that @path@ ambiguously can be matched to each of the @matches@ file paths.
-- Don't let @matches@ equal @[]@.
-- That would be nonsensical and seriously not cool.
-- Don't make me change the type of @matches@ for @['FilePath']@ to 'NonEmpty'.
makeAmbiguousFiles :: Foldable1 f => FileSource -> f FileSource -> InputStreamError
makeAmbiguousFiles path matches = InputStreamError . pure $ FileAmbiguous path (toNonEmpty matches)


-- |
-- Remark that the specified file has no data.
makeEmptyFileStream :: FileSource -> InputStreamError
makeEmptyFileStream = InputStreamError . pure . FileEmptyStream


-- |
-- Remark that the specified file could not be opened (probably a permission error)
makeFileNoReadPermissions :: FileSource -> InputStreamError
makeFileNoReadPermissions = InputStreamError . pure . FileBadPermissions


-- |
-- Remark that the specified file could not be found on the file system
makeFileNotFound :: FileSource -> InputStreamError
makeFileNotFound = InputStreamError . pure . FileUnfindable


-- |
-- Remark that the specified file could not be found on the file system
makeFileInUseOnRead :: FileSource -> InputStreamError
makeFileInUseOnRead = InputStreamError . pure . FileAlreadyInUse
