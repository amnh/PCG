{-# LANGUAGE DoAndIfThenElse #-}

module PCG.Command.Types.Read.Internal
  ( CustomAlphabetOptions(..)
  , CustomAlphabetStrategy(..)
  , module PCG.Command.Types.Read.ReadError
  , FileContent
  , FileResult
  , FileSpecification(..)
  , FileSpecificationContent(..)
  , getSpecifiedContent
  , getSpecifiedTcm
  , eitherTValidation
  ) where

import Control.Monad              (liftM2)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Custom         (eitherTValidation)
import Data.Int
import Data.Text.Lazy             (Text)
import Data.Text.Lazy.IO          (readFile)
import Prelude             hiding (readFile)
import System.Directory
import System.FilePath.Glob

import PCG.Command.Types.Read.ReadError

type FileContent  = Text
type FileResult   = (FilePath, FileContent)

data FileSpecificationContent
   = SpecContent
   { dataFiles :: [FileResult] 
   , tcmFile   :: Maybe FileResult
   } deriving (Eq)

type TcmReference = Maybe FilePath

data FileSpecification
   = UnspecifiedFile    [FilePath] --Try to parse them all?
   | AminoAcidFile      [FilePath]
   | NucleotideFile     [FilePath]
   | AnnotatedFile      [FilePath]
   | ChromosomeFile     [FilePath]
   | GenomeFile         [FilePath]
   | CustomAlphabetFile [FilePath] TcmReference [CustomAlphabetOptions]
   | PrealignedFile     FileSpecification TcmReference
   deriving (Show)

data CustomAlphabetOptions
   = Init3D     Bool
   | Level      Int64 CustomAlphabetStrategy
   | Tiebreaker CustomAlphabetStrategy
   deriving (Show)

data CustomAlphabetStrategy
   = First
   | Last
   | AtRandom
   deriving (Show)

getSpecifiedContent :: FileSpecification -> EitherT ReadError IO FileSpecificationContent
getSpecifiedContent (UnspecifiedFile    xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (AminoAcidFile      xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (NucleotideFile     xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (AnnotatedFile      xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (ChromosomeFile     xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (GenomeFile         xs      ) = getSpecifiedContentSimple xs
getSpecifiedContent (CustomAlphabetFile xs tcm _) = liftM2 SpecContent (getSpecifiedFileContents xs) (getSpecifiedTcm tcm)
getSpecifiedContent (PrealignedFile     fs tcm  ) = do 
    specifiedContent <- getSpecifiedContent fs
    case tcmFile specifiedContent of
      Nothing -> (SpecContent (dataFiles specifiedContent)) <$> (getSpecifiedTcm tcm)
      Just _  -> pure specifiedContent 

getSpecifiedTcm :: Maybe FilePath -> EitherT ReadError IO (Maybe (FilePath, FileContent))
getSpecifiedTcm tcmPath =
    case tcmPath of
      Nothing       -> pure Nothing
      Just tcmPath' -> do
        tcmFiles <- getFileContents tcmPath'
        case tcmFiles of
          [x] -> pure $ Just x
          []  -> left $ unfindable tcmPath'
          _   -> left $ ambiguous tcmPath' (fst <$> tcmFiles)

getSpecifiedFileContents :: [FilePath] -> EitherT ReadError IO [FileResult]
getSpecifiedFileContents = fmap concat . eitherTValidation . fmap getFileContents

getSpecifiedContentSimple :: [FilePath] -> EitherT ReadError IO FileSpecificationContent
getSpecifiedContentSimple = fmap (`SpecContent` Nothing) . getSpecifiedFileContents

-- | Reads in the contents of the given FilePath, correctly interpolating glob paths
getFileContents :: FilePath -> EitherT ReadError IO [(FilePath, FileContent)]
getFileContents path = do
    -- Check if the file exists exactly as specified
    exists <- liftIO $ doesFileExist path
    if   exists
    -- If it exists exactly as specified, read it in
    then pure <$> readFileContent path
    else do
    -- If the file does not exists exactly as specified
    -- try to match other files to the given path
    -- by interpreting the path as a 'glob'
        matches <- liftIO $ glob path
        case matches of
          []  -> left $ unfindable path
          [x] -> pure <$> readFileContent x
          xs  -> eitherTValidation $ readFileContent <$> xs
  where
    readFileContent :: FilePath -> EitherT ReadError IO (FilePath, FileContent)
    readFileContent foundPath = do
        canRead <- liftIO $ readable <$> getPermissions foundPath
        if   not canRead
        then left $ unopenable foundPath
        else do
            content <- liftIO $ readFile foundPath
            pure (foundPath, content)
