{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module PCG.Command.Read.InputStreamError
  ( InputStreamError()
  , makeAmbiguousFiles
  , makeFileNotFound
  , makeFileNotOpenable
  ) where

import Control.DeepSeq         (NFData)
--import Data.Data                 (Data)
import Data.FileSource         (FileSource)
import Data.Foldable
import Data.List.NonEmpty      hiding (toList)
import Data.Maybe              (catMaybes)
import Data.Semigroup.Foldable
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
    deriving stock    (Generic, Show)
    deriving anyclass (NFData)


data  InputStreamErrorMessage
    = FileUnfindable FileSource
    | FileUnopenable FileSource
    | FileAmbiguous  FileSource (NonEmpty FileSource)
    deriving stock    (Generic, Show)
    deriving anyclass (NFData)


instance Semigroup InputStreamError where

    (InputStreamError lhs) <> (InputStreamError rhs) = InputStreamError $ lhs <> rhs


instance TextShow InputStreamError where

    showb (InputStreamError errors) = unlinesB $ catMaybes
        [ unfindableMessage
        , unopenableMessage
        , ambiguousMessage
        ]
      where
        (unfindables, unopenables, ambiguity) = partitionInputStreamErrorMessages $ toList errors

        unfindableMessage =
          case unfindables of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " does not exist"
            xs  -> Just $ "The following files do not exists: \n" <> unlinesB (showb <$> xs)

        unopenableMessage =
          case unopenables of
            []  -> Nothing
            [x] -> Just $ "The file " <> showb x <> " can not be opened"
            xs  -> Just $ "The following files could not be opened: \n" <> unlinesB (showb <$> xs)

        ambiguousMessage  =
          case ambiguity of
            [] -> Nothing
            xs -> Just . unlinesB $ showb <$> xs

        partitionInputStreamErrorMessages
          ::  [InputStreamErrorMessage]
          -> ([InputStreamErrorMessage],[InputStreamErrorMessage], [InputStreamErrorMessage])
        partitionInputStreamErrorMessages = foldr f ([],[],[])
          where
            f e@FileUnfindable    {} (u,v,x) = (e:u,   v,   x)
            f e@FileUnopenable    {} (u,v,x) = (  u, e:v,   x)
            f e@FileAmbiguous     {} (u,v,x) = (  u,   v, e:x)


instance TextShow InputStreamErrorMessage where

    showb (FileUnfindable path        ) = "'" <> showb path <> "'"
    showb (FileUnopenable path        ) = "'" <> showb path <> "'"
    showb (FileAmbiguous  path matches) = message
      where
        files   = toList matches
        message = unlinesB
          [ "The following file specification is ambiguous:"
          , "\t'" <> showb path <> "'"
          , "The file specification should match a single file, but multiple matches were found:"
          , unlinesB $ (\x -> "'" <> showb x <> "'") <$> files
          ]


-- |
-- Remark that the specified file could not be found on the file system
makeFileNotFound :: FileSource -> InputStreamError
makeFileNotFound path = InputStreamError . pure $ FileUnfindable path


-- |
-- Remark that the specified file could not be opened (probably a permission error)
makeFileNotOpenable :: FileSource -> InputStreamError
makeFileNotOpenable path = InputStreamError . pure $ FileUnopenable path


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
