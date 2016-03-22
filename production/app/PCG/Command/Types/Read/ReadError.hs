module PCG.Command.Types.Read.ReadError
  ( ReadError()
  , ambiguous
  , unfindable
  , unopenable
  , unparsable
  ) where

import Data.List.NonEmpty
import Data.Maybe                (catMaybes)
import Data.Semigroup
import Text.Megaparsec           (ParseError)

-- | The various ways in which a 'Read' 'Command' from a POY script can fail.
-- A single 'Read' 'Command' can fail in multiple ways simultaneously.
-- To account for this the 'ReadError' type is a composable 'Semigroup' to allow for the collection of possible sub errors to be coalesced into a single 'ReadError' value. The `show` definition will render the 'Read Error' as a human legible collection of errors that occured within the 'Read' 'Command'.
data ReadError = ReadError (NonEmpty ReadErrorMessage)

data ReadErrorMessage
   = FileUnfindable FilePath
   | FileUnopenable FilePath
   | FileUnparsable ParseError
   | FileAmbiguous  FilePath (NonEmpty FilePath)

instance Show ReadErrorMessage where
  show (FileUnfindable path        ) = "'" ++ path ++ "'"
  show (FileUnopenable path        ) = "'" ++ path ++ "'"
  show (FileUnparsable pErr        ) = show pErr
  show (FileAmbiguous  path matches) = message
    where
      files   = toList matches
      message = unlines
        [ "The following file specification is ambiguous:"
        , "\t'"++path++"'"
        , "The file specification should match a single file, but multiple matches were found:"
        , unlines $ (\x -> '\'' : x ++ "'") <$> files
        ]

instance Semigroup ReadError where
  (ReadError lhs) <> (ReadError rhs) = ReadError $ lhs <> rhs

instance Show ReadError where
  show (ReadError errors) = unlines $ catMaybes [unfindableMessage, unopenableMessage, unparsableMessage, ambiguousMessage]
    where
      (unfindables,unopenables,unparsables,ambiguity) = partitionReadErrorMessages $ toList errors
      unfindableMessage =
        case unfindables of
          []  -> Nothing
          [x] -> Just $ "The file "  ++ show x ++ " does not exist"
          xs  -> Just $ "The following files do not exists: \n"        ++ unlines (show <$> xs)
      unopenableMessage =
        case unopenables of
          []  -> Nothing
          [x] -> Just $ "The file "  ++ show x ++ " can not be openned"
          xs  -> Just $ "The following files could not be openned: \n" ++ unlines (show <$> xs)
      unparsableMessage =
        case unparsables of
          []  -> Nothing
          xs  -> Just . unlines $ (\x -> "Parse Error:\n" ++ show x) <$> xs
      ambiguousMessage  =
        case ambiguity of
          []  -> Nothing
          xs  -> Just . unlines $ show <$> xs
      partitionReadErrorMessages :: [ReadErrorMessage] -> ([ReadErrorMessage], [ReadErrorMessage], [ReadErrorMessage], [ReadErrorMessage])
      partitionReadErrorMessages = foldr f ([],[],[],[])
        where
          f e@(FileUnfindable _  ) (w,x,y,z) = (e:w,  x,  y,  z) 
          f e@(FileUnopenable _  ) (w,x,y,z) = (  w,e:x,  y,  z) 
          f e@(FileUnparsable _  ) (w,x,y,z) = (  w,  x,e:y,  z) 
          f e@(FileAmbiguous  _ _) (w,x,y,z) = (  w,  x,  y,e:z)

-- | Remark that the specified file could not be found on the file system
unfindable :: FilePath -> ReadError
unfindable path = ReadError $ FileUnfindable path :| []

-- | Remark that the specified file could not be opened (probably a permission error)
unopenable :: FilePath -> ReadError
unopenable path = ReadError $ FileUnopenable path :| []

-- | Remark that a parsing error occured when reading the file. Note that the 'ParseError' should contain the 'FilePath' information. 
unparsable :: ParseError -> ReadError
unparsable pErr = ReadError $ FileUnparsable pErr :| []

-- | Remark that the specified file path matches many possible files.
-- This should be used when a single file is expected but 'regex matching' or 'file globbing'
-- are present in the processing of the Read Command.
--
-- @ambiguous path matches@ notes that @path@ ambiguously can be matched to each of the @matches@ file paths.
-- Don't let @matches@ equal @[]@.
-- That would be nonsensical and seriously not cool.
-- Don't make me change the type of @matches@ for @['FilePath']@ to 'NonEmpty'.
ambiguous  :: FilePath -> [FilePath] -> ReadError
ambiguous path matches = ReadError $ FileAmbiguous path (fromList matches) :| []
