{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Read.ReadError
  ( ReadError
  , ambiguous
  , invalidPrealigned
  , unfindable
  , unopenable
  , unparsable
  ) where

import Data.List.NonEmpty
import Data.Maybe              (catMaybes)
import Data.Semigroup.Foldable
import Text.Megaparsec


-- |
-- The various ways in which a 'Read' 'Command' from a POY script can fail.
-- A single 'Read' 'Command' can fail in multiple ways simultaneously.
-- To account for this the 'ReadError' type is a composable 'Semigroup' to allow for the collection of possible sub errors to be coalesced into a single 'ReadError' value. The `show` definition will render the 'Read Error' as a human legible collection of errors that occured within the 'Read' 'Command'.
newtype ReadError = ReadError (NonEmpty ReadErrorMessage)


data ReadErrorMessage
   = FileUnfindable    FilePath
   | FileUnopenable    FilePath
   | FileUnparsable    String
   | FileAmbiguous     FilePath (NonEmpty FilePath)
   | InvalidPrealigned FilePath (NonEmpty Word)


instance Semigroup ReadError where

    (ReadError lhs) <> (ReadError rhs) = ReadError $ lhs <> rhs


instance Show ReadError where

    show (ReadError errors) = unlines $ catMaybes
        [ unfindableMessage
        , unopenableMessage
        , unparsableMessage
        , ambiguousMessage
        , invalidPrealignsMessage
        ]
      where
        (unfindables, unopenables, unparsables, ambiguity, invalidPrealigns) = partitionReadErrorMessages $ toList errors

        unfindableMessage =
          case unfindables of
            []  -> Nothing
            [x] -> Just $ "The file "  <> show x <> " does not exist"
            xs  -> Just $ "The following files do not exists: \n"        <> unlines (show <$> xs)

        unopenableMessage =
          case unopenables of
            []  -> Nothing
            [x] -> Just $ "The file "  <> show x <> " can not be opened"
            xs  -> Just $ "The following files could not be opened: \n" <> unlines (show <$> xs)

        unparsableMessage =
          case unparsables of
            []  -> Nothing
            [x] -> Just $  "Could not parse file " <> show x
            xs  -> Just . ("Could not parse the following files:\n" <>) . unlines $ show <$> xs

        ambiguousMessage  =
          case ambiguity of
            [] -> Nothing
            xs -> Just . unlines $ show <$> xs

        invalidPrealignsMessage =
          case invalidPrealigns of
            []  -> Nothing
            [x] -> Just $ "The file was specified as prealigned, but not all characters had the same length. " <> show x
            xs  -> Just $ "The following files were specified as prealigned, but not all characters had the same length: \n" <> unlines (show <$> xs)

        partitionReadErrorMessages
          ::  [ReadErrorMessage]
          -> ([ReadErrorMessage],[ReadErrorMessage], [ReadErrorMessage], [ReadErrorMessage], [ReadErrorMessage])
        partitionReadErrorMessages = foldr f ([],[],[],[],[])
          where
            f e@FileUnfindable    {} (u,v,w,x,z) = (e:u,  v,  w,  x,  z)
            f e@FileUnopenable    {} (u,v,w,x,z) = (  u,e:v,  w,  x,  z)
            f e@FileUnparsable    {} (u,v,w,x,z) = (  u,  v,e:w,  x,  z)
            f e@FileAmbiguous     {} (u,v,w,x,z) = (  u,  v,  w,e:x,  z)
            f e@InvalidPrealigned {} (u,v,w,x,z) = (  u,  v,  w,  x,e:z)


instance Show ReadErrorMessage where

    show (FileUnfindable    path) = "'" <> path <> "'"
    show (FileUnopenable    path) = "'" <> path <> "'"
    show (FileUnparsable    pErr) = pErr
    show (InvalidPrealigned path cols) = mconcat ["'", path, "', has characters of lengths ", fold1 . intersperse ", " $ show <$> cols]
    show (FileAmbiguous path matches) = message
      where
        files   = toList matches
        message = unlines
          [ "The following file specification is ambiguous:"
          , "\t'"<>path<>"'"
          , "The file specification should match a single file, but multiple matches were found:"
          , unlines $ (\x -> '\'' : x <> "'") <$> files
          ]


-- |
-- Remark that the specified file could not be found on the file system
unfindable :: FilePath -> ReadError
unfindable path = ReadError $ FileUnfindable path :| []


-- |
-- Remark that the specified file could not be opened (probably a permission error)
unopenable :: FilePath -> ReadError
unopenable path = ReadError $ FileUnopenable path :| []


-- |
-- Remark that a parsing error occured when reading the file. Note that the 'ParseError' should contain the 'FilePath' information.
unparsable :: (ShowToken (Token s), LineToken (Token s), ShowErrorComponent e, Stream s) => s -> ParseError (Token s) e -> ReadError
unparsable pStr pErr = ReadError $ FileUnparsable (parseErrorPretty' pStr pErr) :| []


-- |
-- Remark that the specified file path matches many possible files.
-- This should be used when a single file is expected but 'regex matching' or 'file globbing'
-- are present in the processing of the Read Command.
--
-- @ambiguous path matches@ notes that @path@ ambiguously can be matched to each of the @matches@ file paths.
-- Don't let @matches@ equal @[]@.
-- That would be nonsensical and seriously not cool.
-- Don't make me change the type of @matches@ for @['FilePath']@ to 'NonEmpty'.
ambiguous :: Foldable1 f => FilePath -> f FilePath -> ReadError
ambiguous path matches = ReadError $ FileAmbiguous path (toNonEmpty matches) :| []


invalidPrealigned :: Integral i => FilePath -> NonEmpty i -> ReadError
invalidPrealigned path = ReadError . pure . InvalidPrealigned path . fmap (fromIntegral . abs)
