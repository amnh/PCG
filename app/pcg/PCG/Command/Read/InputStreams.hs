{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.FileSource
import           Data.Functor
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Text                                 (Text)
import           Data.Text.IO                              (readFile)
import           Data.Validation
import           PCG.Command.Read
import           PCG.Command.Read.ReadCommandError
import           Prelude                                   hiding (readFile)
import           System.Directory
import           System.FilePath.Glob


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


{-
-- |
-- An optional reference to a TCM file.
type  TcmReference = FileSource
-}


{-
evaluate :: ReadCommand -> SearchState
evaluate (ReadCommand fileSpecs) = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    -- TODO: use Validation here.
    readResult <- liftIO $ parmap rpar (fmap removeGaps . parseSpecifiedFile) fileSpecs
    case readResult of
      Failure rErr -> failWithPhase Reading rErr
      Success rRes -> do
        parseResult <- liftIO $ parmap rpar (fmap removeGaps . parseSpecifiedFile) rRes
        case parseResult of
          Failure pErr -> failWithPhase Parsing pErr
          Success pRes ->
            case decoration . unifyPartialInputs $ transformation <$> fold1 pRes of
              Failure uErr -> failWithPhase Unifying uErr   -- Report structural errors here.
              -- TODO: rectify against 'old' SearchState, don't just blindly merge or ignore old state
              Success g ->  liftIO $ compact g
                         -- liftIO (putStrLn "DECORATION CALL:" *> print g) *> pure g
                         -- (liftIO . putStrLn {- . take 500000 -} $ either show (ppTopElement . toXML) g)  
                         -- (liftIO . putStrLn $ show g) $> g
  where
    transformation = id -- expandIUPAC
    decoration     = fmap (fmap initializeDecorations2)
-}


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
{-    
    fs'  <- getSpecifiedContent fs
    tcm' <- getSpecifiedTcm tcm    
    pure $ fs' `bindValidation` (\(SpecContent fs'') ->
               tcm' `bindValidation` (\tcm'' ->
                   pure . SpecContent $ (DataContent <$> dataFile <*> const (Just tcm'')) <$> fs''
                                     )
                                )
-}
getSpecifiedContent (CustomAlphabetFile xs tcm) = do
    xs'  <- getSpecifiedFileContents xs
    tcm' <- getSpecifiedTcm tcm
    pure . SpecContent $ (`DataContent` Just tcm') <$> xs'
{-
    xs'  <- getSpecifiedFileContents xs
    tcm' <- getSpecifiedTcm tcm
    pure $ xs' `bindValidation` (\xs'' ->
               tcm' `bindValidation` (\tcm'' ->
                   pure . SpecContent $ (`DataContent` Just tcm'') <$> xs''
                                     )
                                )
-}

  
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

{-
traverseValidationT
  :: (Applicative f, Semigroup e, Traversable t)
  => (a -> ValidationT e f b)
  -> t a
  -> ValidationT e f (t b)
traverseValidationT f = ValidationT . fmap sequenceA . traverse f
-}


data  ValidationT e m a
    = ValidationT { runValidationT :: m (Validation e a) }


instance Functor m => Functor (ValidationT e m) where

    fmap f = ValidationT . fmap (fmap f). runValidationT


instance (Applicative m, Semigroup e) => Applicative (ValidationT e m) where

    pure = ValidationT . pure . Success

    (<*>) f v = ValidationT $ ((<*>) <$> runValidationT f) <*> runValidationT v


instance (Monad m, Semigroup e) => Monad (ValidationT e m) where

    return = pure

    (>>=) v f = ValidationT $ do
        x <- runValidationT v
        case x of
          Failure e -> pure $ Failure e
          Success a -> runValidationT $ f a


instance Semigroup e => MonadIO (ValidationT e IO) where

    liftIO = ValidationT . fmap Success


invalid :: Applicative f => e -> ValidationT e f a
invalid = ValidationT . pure . Failure
