-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource.OutputStreamError
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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnboxedSums        #-}

module Data.FileSource.OutputStreamError
  ( OutputStreamError()
  , makeFileInUseOnWrite
  , makeFileNoWritePermissions
  , makePathDoesNotExist
  , makeNotEnoughSpace
  ) where

import Control.DeepSeq    (NFData)
import Data.Data
import Data.FileSource
import Data.Foldable
import Data.List.NonEmpty hiding (toList)
import Data.Maybe         (catMaybes)
import GHC.Generics       (Generic)
import TextShow


-- |
-- The various ways in which writing output data from PCG can fail.
--
-- A single file can fail at outputting data in multiple ways, /sometimes simultaneously/.
-- To account for this the 'OutputStreamError' type is a composable 'Semigroup' to allow
-- for the collection of many possible outputting errors to be coalesced into a single
-- 'OutputStreamError' value.
--
-- The 'TextShow' instance should be used to render the 'OutputStreamError' as a human legible
-- collection of output errors that occurred while attempting to output data from PCG.
--
-- The 'Show' instance should only be used for debugging purposes.
newtype OutputStreamError = OutputStreamError (NonEmpty OutputStreamErrorMessage)
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


data  OutputStreamErrorMessage
    = FileAlreadyInUse {-# UNPACK #-} !FileSource
    | PathDoesNotExist {-# UNPACK #-} !FileSource
    | NoPermissions    {-# UNPACK #-} !FileSource
    | NotEnoughSpace   {-# UNPACK #-} !FileSource
    deriving stock    (Data, Generic, Show, Typeable)
    deriving anyclass (NFData)


instance Semigroup OutputStreamError where

    (OutputStreamError lhs) <> (OutputStreamError rhs) = OutputStreamError $ lhs <> rhs


instance TextShow OutputStreamError where

    showb (OutputStreamError errors) = unlinesB $ catMaybes
        [ lockedFilesMessage
        , missingPathMessage
        , badPermissionMessage
        , noSpaceMessage
        ]
      where
        (lockedFiles, missingPaths, badPermissions, noSpaceErrors) = partitionOutputStreamErrorMessages $ toList errors

        lockedFilesMessage =
          case lockedFiles of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " was locked"
            xs  -> Just $ "The following files were locked and could not be written to: \n" <> unlinesB (showb <$> xs)

        missingPathMessage =
          case missingPaths of
            []  -> Nothing
            [x] -> Just $ "The output file path " <> showb x <> " could not be found"
            xs  -> Just $ "The following output file paths cound not be found:\n" <> unlinesB  (showb <$> xs)

        badPermissionMessage =
          case badPermissions of
            []  -> Nothing
            [x] -> Just $ "The file path " <> showb x <> " had premissions which prevent it from being written to"
            xs  -> Just $ "The following file paths had premissions which prevent them from being written to:\n" <> unlinesB  (showb <$> xs)

        noSpaceMessage =
          case noSpaceErrors of
            []  -> Nothing
            [x] -> Just $ "There is was not enough space to write the file " <> showb x <> "."
            xs  -> Just $ "There was not enough disk space to write the following files:\n" <> unlinesB  (showb <$> xs)

        partitionOutputStreamErrorMessages
          ::  [OutputStreamErrorMessage]
          -> ([OutputStreamErrorMessage]
             ,[OutputStreamErrorMessage]
             ,[OutputStreamErrorMessage]
             ,[OutputStreamErrorMessage]
             )
        partitionOutputStreamErrorMessages = foldr f ([],[],[],[])
          where
            f e@FileAlreadyInUse{} (v,x,y,z) = (e:v,   x,   y,   z)
            f e@PathDoesNotExist{} (v,x,y,z) = (  v, e:x,   y,   z)
            f e@NoPermissions   {} (v,x,y,z) = (  v,   x, e:y,   z)
            f e@NotEnoughSpace  {} (v,x,y,z) = (  v,   x,   y, e:z)


instance TextShow OutputStreamErrorMessage where

    showb (FileAlreadyInUse path) = "'" <> showb path <> "'"
    showb (PathDoesNotExist path) = "'" <> showb path <> "'"
    showb (NoPermissions    path) = "'" <> showb path <> "'"
    showb (NotEnoughSpace   path) = "'" <> showb path <> "'"


-- |
-- Remark that the file is in use when attempting to write output.
makeFileInUseOnWrite :: FileSource -> OutputStreamError
makeFileInUseOnWrite = OutputStreamError . pure . FileAlreadyInUse


-- |
-- Remark that the file permissions do not allow output data to be written to the file.
makeFileNoWritePermissions :: FileSource -> OutputStreamError
makeFileNoWritePermissions = OutputStreamError . pure . NoPermissions


-- |
-- Remark that the file path does not exist.
makePathDoesNotExist :: FileSource -> OutputStreamError
makePathDoesNotExist = OutputStreamError . pure . PathDoesNotExist


-- |
-- Remark that there is not a space on disk when outputting data the file.
makeNotEnoughSpace :: FileSource -> OutputStreamError
makeNotEnoughSpace = OutputStreamError . pure . NotEnoughSpace
