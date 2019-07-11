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
-- Composable error type representing failure to reterive an input data stream.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnboxedSums        #-}

module Data.FileSource.InputStreamError
  ( InputStreamError()
  , makeAmbiguousFiles
  , makeEmptyFileStream
  , makeFileInUseOnRead
  , makeFileNoReadPermissions
  , makeFileNotFound
  ) where

import Control.DeepSeq         (NFData)
import Data.FileSource
import Data.Foldable
import Data.List.NonEmpty      hiding (toList)
import Data.Maybe              (catMaybes)
import Data.Semigroup.Foldable
import GHC.Generics            (Generic)
import TextShow


-- |
-- The various ways in which reading input data into PCG can fail.
--
-- A single file can fail at inputing data in multiple ways, /sometimes simultaneously/.
-- To account for this the 'InputStreamError' type is a composable 'Semigroup' to allow
-- for the collection of many possible inputing errors to be coalesced into a single
-- 'InputStreamError' value.
--
-- The 'TextShow' instance should be used to render the 'InputStreamError' as a human legible
-- collection of input errors that occured while attempting to input data into PCG.
--
-- The 'Show' instance should only be used for debugging purposes.
newtype InputStreamError = InputStreamError (NonEmpty InputStreamErrorMessage)
    deriving stock    (Generic, Show)
    deriving anyclass (NFData)


data  InputStreamErrorMessage
    = FileAlreadyInUse   {-# UNPACK #-} !FileSource
    | FileAmbiguous      {-# UNPACK #-} !FileSource {-# UNPACK #-} !(NonEmpty FileSource)
    | FileBadPermissions {-# UNPACK #-} !FileSource
    | FileEmptyStream    {-# UNPACK #-} !FileSource
    | FileUnfindable     {-# UNPACK #-} !FileSource
    deriving stock    (Generic, Show)
    deriving anyclass (NFData)


instance Semigroup InputStreamError where

    (InputStreamError lhs) <> (InputStreamError rhs) = InputStreamError $ lhs <> rhs


instance TextShow InputStreamError where

    showb (InputStreamError errors) = unlinesB $ catMaybes
        [ unfindableMessage
        , ambiguousMessage
        , unopenableMessage
        , alreadInUseMessage
        , emptyStreamsMessage
        ]
      where
        (inUseFiles, ambiguity, badPermissions, emptyStreams, unfindables) = partitionInputStreamErrorMessages $ toList errors

        alreadInUseMessage =
          case inUseFiles of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " is already in use"
            xs  -> Just $ "The following files were already in use:\n" <> unlinesB (showb <$> xs)

        ambiguousMessage =
          case ambiguity of
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
             )
        partitionInputStreamErrorMessages = foldr f ([],[],[],[],[])
          where
            f e@FileAlreadyInUse   {} (v,w,x,y,z) = (e:v,   w,   x,   y,   z)
            f e@FileAmbiguous      {} (v,w,x,y,z) = (  v, e:w,   x,   y,   z)
            f e@FileBadPermissions {} (v,w,x,y,z) = (  v,   w, e:x,   y,   z)
            f e@FileEmptyStream    {} (v,w,x,y,z) = (  v,   w,   x, e:y,   z)
            f e@FileUnfindable     {} (v,w,x,y,z) = (  v,   w,   x,   y, e:z)


instance TextShow InputStreamErrorMessage where

    showb (FileAlreadyInUse   path        ) = "'" <> showb path <> "'"
    showb (FileEmptyStream    path        ) = "'" <> showb path <> "'"
    showb (FileUnfindable     path        ) = "'" <> showb path <> "'"
    showb (FileBadPermissions path        ) = "'" <> showb path <> "'"
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
