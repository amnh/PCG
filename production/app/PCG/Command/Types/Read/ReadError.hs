module PCG.Command.Types.Read.ReadError
  ( ReadError()
  , ambiguous
  , unfindable
  , unopenable
  , unparsable
  ) where

import Data.List.NonEmpty hiding (unlines)
import Data.Maybe                (catMaybes)
import Data.Semigroup
import Text.Megaparsec           (ParseError)

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
      message = unlines $
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
          xs  -> Just $ "The following files do not exists: \n"        ++ (unlines $ show <$> xs)
      unopenableMessage =
        case unopenables of
          []  -> Nothing
          [x] -> Just $ "The file "  ++ show x ++ " can not be openned"
          xs  -> Just $ "The following files could not be oppened: \n" ++ (unlines $ show <$> xs)
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

unfindable :: FilePath -> ReadError
unfindable path = ReadError $ FileUnfindable path :| []

unopenable :: FilePath -> ReadError
unopenable path = ReadError $ FileUnopenable path :| []

unparsable :: ParseError -> ReadError
unparsable pErr = ReadError $ FileUnparsable pErr :| []

ambiguous  :: FilePath -> [FilePath] -> ReadError
ambiguous path matches = ReadError $ FileAmbiguous path (fromList matches) :| []
