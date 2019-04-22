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
-- Exposes several useful disk utility related functionality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.FileSource.OutputStreamError
  ( OutputStreamError()
  , makeFileInUseOnWrite
  , makeFileNoWritePermissions
  , makeFileUnwritable
  , makePathDoesNotExist
  , makeNotEnoughSpace
  ) where

import Control.DeepSeq    (NFData)
import Data.Bifunctor     (first)
--import           Data.Coerce               (Coercible, coerce)
--import           Data.Data                 (Data)
import Data.FileSource
import Data.Foldable
import Data.List.NonEmpty hiding (toList)
import Data.Maybe         (catMaybes)
import GHC.Generics       (Generic)
import TextShow


newtype OutputStreamError = OutputStreamError (NonEmpty OutputStreamErrorMessage)
    deriving (Generic, NFData, Show)


data  OutputStreamErrorMessage
    = FileUnwritable   FileSource
    | FileAlreadyInUse FileSource
    | PathDoesNotExist FileSource
    | NoPermissions    FileSource
    | NotEnoughSpace   FileSource
    deriving (Generic, NFData, Show)


instance Semigroup OutputStreamError where

    (OutputStreamError lhs) <> (OutputStreamError rhs) = OutputStreamError $ lhs <> rhs


instance TextShow OutputStreamError where

    showb (OutputStreamError errors) = unlinesB $ catMaybes
        [ unwritableMessage
        , lockedFilesMessage
        , noSpaceMessage
        ]
      where
        (unwritables, lockedFiles, noSpaceErrors) = partitionOutputStreamErrorMessages $ toList errors

        unwritableMessage =
          case unwritables of
            []  -> Nothing
            [x] -> Just $ "The file path" <> showb x <> " was not writable."
            xs  -> Just $ "The following files were not writable: \n" <> unlinesB (showb <$> xs)

        lockedFilesMessage =
          case lockedFiles of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " was locked."
            xs  -> Just $ "The following files were locked and could not be written to: \n" <> unlinesB (showb <$> xs)

        noSpaceMessage =
          case noSpaceErrors of
            []  -> Nothing
            [x] -> Just $ "There is was not enough space to write the file " <> showb x <> "."
            xs  -> Just $ "There was not enough disk space to write the following files:\n" <> unlinesB  (showb <$> xs)

        partitionOutputStreamErrorMessages
          ::  [OutputStreamErrorMessage]
          -> ([OutputStreamErrorMessage],[OutputStreamErrorMessage], [OutputStreamErrorMessage])
        partitionOutputStreamErrorMessages = foldr f ([],[],[])
          where
            f e@FileUnwritable  {} (u,v,x) = (e:u,   v,   x)
            f e@FileAlreadyInUse{} (u,v,x) = (  u, e:v,   x)
            f e@NotEnoughSpace  {} (u,v,x) = (  u,   v, e:x)


instance TextShow OutputStreamErrorMessage where

    showb (FileUnwritable   path) = "'" <> showb path <> "'"
    showb (FileAlreadyInUse path) = "'" <> showb path <> "'"
    showb (NotEnoughSpace   path) = "'" <> showb path <> "'"


makeFileInUseOnWrite :: FileSource -> OutputStreamError
makeFileInUseOnWrite = OutputStreamError . pure . FileAlreadyInUse


makeFileNoWritePermissions :: FileSource -> OutputStreamError
makeFileNoWritePermissions = OutputStreamError . pure . NoPermissions


makeFileUnwritable :: FileSource -> OutputStreamError
makeFileUnwritable = OutputStreamError . pure . FileUnwritable


makePathDoesNotExist :: FileSource -> OutputStreamError
makePathDoesNotExist = OutputStreamError . pure . PathDoesNotExist


makeNotEnoughSpace :: FileSource -> OutputStreamError
makeNotEnoughSpace = OutputStreamError . pure . NotEnoughSpace
