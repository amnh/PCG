{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Read.InputStreams
  ( DataContent(..)
  , FileSpecificationContent(..)
  , FileContent
  , FileResult
  , TcmReference
  , ValidationT(..)
  , getSpecifiedContent
  , getSpecifiedTcm
  , invalid
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Trans.Validation
import Data.FileSource
import Data.Functor
import Data.List.NonEmpty                (NonEmpty (..))
import Data.MonoTraversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Text                         (Text)
import Data.Text.IO                      (readFile)
import Data.Validation
import PCG.Command.Read
import PCG.Command.Read.ReadCommandError
import Prelude                           hiding (readFile)
import System.Directory
import System.FilePath.Glob


-- |
-- The collection of file content collected from a 'FileSpecification'.
newtype FileSpecificationContent = SpecContent (NonEmpty DataContent)


-- |
-- Content of a single data file along with a possibly associated TCM file content.
data  DataContent
    = DataContent
    { dataFile :: !FileResult
    , tcmFile  :: !(Maybe FileResult)
    } deriving (Eq)


-- |
-- The content of a file.
type  FileContent  = Text


-- |
-- The context of reading a file along with the path the content originated from.
type  FileResult   = (FileSource, FileContent)


getSpecifiedContent :: FileSpecification -> ValidationT ReadCommandError IO FileSpecificationContent
getSpecifiedContent (UnspecifiedFile    xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (AminoAcidFile      xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (NucleotideFile     xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (AnnotatedFile      xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (ChromosomeFile     xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (GenomeFile         xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (PrealignedFile     fs    ) = getSpecifiedContent fs
getSpecifiedContent (WithSpecifiedTCM   fs tcm) = do
    SpecContent fs'  <- getSpecifiedContent fs
    tcm'             <- getSpecifiedTcm tcm
    pure . SpecContent $ (DataContent <$> dataFile <*> const (Just tcm')) <$> fs'
getSpecifiedContent (CustomAlphabetFile xs tcm) = do
    xs'  <- getSpecifiedFileContents xs
    tcm' <- getSpecifiedTcm tcm
    pure . SpecContent $ (`DataContent` Just tcm') <$> xs'


getSpecifiedTcm :: FileSource -> ValidationT ReadCommandError IO FileResult
getSpecifiedTcm tcmPath = getFileContents tcmPath >>= f
  where
    f = \case
           x:|[] -> pure x
           xs    -> invalid . ambiguous tcmPath $ fst <$> xs


getSpecifiedFileContents :: (Foldable1 f, Traversable f) => f FileSource -> ValidationT ReadCommandError IO (NonEmpty FileResult)
getSpecifiedFileContents = fmap fold1 . traverse getFileContents


getSpecifiedContentSimple :: (Foldable1 f, Traversable f) => f FileSource -> ValidationT ReadCommandError IO FileSpecificationContent
getSpecifiedContentSimple = fmap (SpecContent . fmap (`DataContent` Nothing)) . getSpecifiedFileContents


-- |
-- Reads in the contents of the given FilePath, correctly interpolating glob paths
getFileContents :: FileSource -> ValidationT ReadCommandError IO (NonEmpty FileResult)
getFileContents path = do
    -- Check if the file exists exactly as specified
    exists <- liftIO $ doesFileExist filePath
    if   exists
    -- If it exists exactly as specified, read it in
    then pure <$> readFileContent filePath
    else do
    -- If the file does not exists exactly as specified
    -- try to match other files to the given path
    -- by interpreting the path as a 'glob'
        matches <- liftIO $ glob filePath
        case matches of
          []   -> invalid $ unfindable path
          [x]  -> pure <$> readFileContent x
          x:xs -> traverse readFileContent $ x:|xs
  where
    filePath = force $ otoList path

    readFileContent :: FilePath -> ValidationT ReadCommandError IO FileResult
    readFileContent foundPath = do
        canRead <- liftIO $ readable <$> getPermissions foundPath
        if   not canRead
        then invalid $ unopenable path
        else do
            content <- liftIO $ readFile foundPath
            pure (path, content)
