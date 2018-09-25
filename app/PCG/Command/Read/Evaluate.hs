{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Character.Parsed
import           Bio.Graph
import           Bio.Graph.Forest.Parsed
import           Bio.Metadata.Parsed
import           Control.Monad                             (liftM2, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Alphabet
import           Data.Bifunctor                            (bimap, first)
import           Data.Compact                              (compact)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Functor
import           Data.Key
import           Data.List                                 (sortOn)
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NE
import           Data.List.Utility                         (occurances)
import           Data.Map                                  (Map)
import           Data.Maybe                                (catMaybes)
import           Data.Ord                                  (comparing)
import           Data.TCM                                  (TCMDiagnosis (..), TCMStructure (..), diagnoseTcm)
import qualified Data.TCM                                  as TCM
import           Data.Text.IO                              (readFile)
import           Data.Validation
import qualified Data.Vector                               as V
import           Data.Void
import           File.Format.Dot
import           File.Format.Fasta                         hiding (FastaSequenceType (..))
import qualified File.Format.Fasta                         as Fasta (FastaSequenceType (..))
import           File.Format.Fastc                         hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                         (nexusStreamParser)
import           File.Format.TNT                           hiding (weight)
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
import           PCG.Command.Read
import           PCG.Command.Read.DecorationInitialization
import           PCG.Command.Read.ReadError
import           PCG.Command.Read.Unification.Master
import           PCG.Syntax                                (Command (..))
import           Prelude                                   hiding (readFile)
import           System.Directory
import           System.FilePath.Glob
import           Text.Megaparsec


parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
parse' = parse


evaluate :: ReadCommand -> SearchState
evaluate (ReadCommand fileSpecs) = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runExceptT . eitherTValidation $ parmap rpar (fmap removeGaps . parseSpecifiedFile) fileSpecs
--    liftIO $ print result
    case result of
      Left pErr -> fail $ show pErr   -- Report structural errors here.
      Right xs ->
        case decoration . masterUnify $ transformation <$> concat xs of
          Left uErr -> fail $ show uErr -- Report unification errors here.
           -- TODO: rectify against 'old' SearchState, don't just blindly merge or ignore old state
          Right g   -> liftIO (compact g)
                       -- liftIO (putStrLn "DECORATION CALL:" *> print g) *> pure g
                       -- (liftIO . putStrLn {- . take 500000 -} $ either show (ppTopElement . toXML) g)
                         -- (liftIO . putStrLn $ show g) $> g
  where
    transformation = id -- expandIUPAC
    decoration     = fmap (fmap initializeDecorations2)


removeGaps :: Functor f => f FracturedParseResult -> f FracturedParseResult
removeGaps = fmap removeGapsFromDynamicCharsNotMarkedAsAligned


parseSpecifiedFile :: FileSpecification -> ExceptT ReadError IO [FracturedParseResult]
parseSpecifiedFile      AnnotatedFile     {}     = fail "Annotated file specification is not implemented"
parseSpecifiedFile      ChromosomeFile    {}     = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      GenomeFile        {}     = fail "Genome file specification is not implemented"
parseSpecifiedFile spec@AminoAcidFile     {}     = fastaAminoAcid spec
parseSpecifiedFile spec@NucleotideFile    {}     = fastaDNA       spec
parseSpecifiedFile spec@CustomAlphabetFile{}     = parseCustomAlphabet spec
parseSpecifiedFile spec@(UnspecifiedFile      _) =
  getSpecifiedContent spec >>= eitherTValidation . fmap (progressiveParse . fst) . dataFiles
parseSpecifiedFile     (PrealignedFile x tcmRef) = do
    tcmContent <- getSpecifiedTcm tcmRef
    subContent <- parseSpecifiedFile x
    combined   <- case tcmContent of
                   Nothing              -> pure subContent
                   Just (path, content) -> do
                     tcmMat <- ExceptT . pure . first (unparsable content) $ parse' tcmStreamParser path content
                     traverse (ExceptT . pure . setTcm tcmMat path) subContent
    ExceptT . pure . toEither . sequenceA $ expandDynamicCharsMarkedAsAligned . setCharactersToAligned <$> combined



setTcm :: TCM -> FilePath -> FracturedParseResult -> Either ReadError FracturedParseResult
setTcm t tcmPath fpr =
   case relatedTcm fpr of
     Just _  -> Left $ multipleTCMs (sourceFile fpr) tcmPath
     Nothing ->
       let (coefficient, resultTCM, structure) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
           (unfactoredWeight, unfactoredTCM)   = TCM.fromList . toList $ transitionCosts t
           relatedAlphabet                     = fromSymbols $ customAlphabet t
           metadataUpdate x = x
               { weight   = weight x * fromRational unfactoredWeight * fromIntegral coefficient
               , alphabet = relatedAlphabet
               }
       in  pure $ fpr
           { parsedMetas = metadataUpdate <$> parsedMetas fpr
           , relatedTcm  = Just (resultTCM, structure)
           }


fastaDNA :: FileSpecification -> ExceptT ReadError IO [FracturedParseResult]
--fastaDNA spec | trace ("fasta DNA parser with spec " ++ show spec) False = undefined
fastaDNA spec = getSpecifiedContent spec >>= (ExceptT . pure . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = {- (\x -> trace (show x) x) . -} first (unparsable content) $ parse' combinator path content
        combinator  = (\x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x) =<< fastaStreamParser


-- TODO: abstract these two (three) v^
fastaAminoAcid :: FileSpecification -> ExceptT ReadError IO [FracturedParseResult]
fastaAminoAcid spec = getSpecifiedContent spec >>= (ExceptT . pure . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first (unparsable content) $ parse' combinator path content
        combinator  = fastaStreamConverter Fasta.AminoAcid =<< fastaStreamParser


parseSpecifiedContent :: (FileResult -> Either ReadError FracturedParseResult) -> FileSpecificationContent -> Either ReadError [FracturedParseResult]
parseSpecifiedContent parse'' = eitherValidation . fmap parse'' . dataFiles


parseCustomAlphabet :: FileSpecification -> ExceptT ReadError IO [FracturedParseResult]
parseCustomAlphabet spec = getSpecifiedContent spec >>= (ExceptT . pure . parseSpecifiedContentWithTcm)
  where
    parse'' m (path, content) =
        case m of
          Nothing     -> fracturedResult
          Just oldTCM -> setTcm oldTCM path =<< fracturedResult
      where
        fracturedResult = first (unparsable content) $ parse' (try fastaCombinator <|> fastcCombinator) path content
        fastcCombinator = fmap (toFractured Nothing path) fastcStreamParser
        fastaCombinator = fmap (toFractured Nothing path) $
                          fastaStreamParser >>=
                          (\x -> try (fastaStreamConverter Fasta.DNA       x)
                             <|> try (fastaStreamConverter Fasta.RNA       x)
                             <|>      fastaStreamConverter Fasta.AminoAcid x)

    parseSpecifiedContentWithTcm :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContentWithTcm specContent = do
        tcmMay <-
          case tcmFile specContent of
            Nothing              -> pure Nothing
            Just (path, content) -> bimap (unparsable content) Just $ parse' tcmStreamParser path content
        eitherValidation . fmap (parse'' tcmMay) $ dataFiles specContent


-- TODO: check file extension, to guess which parser to use first
progressiveParse :: FilePath -> ExceptT ReadError IO FracturedParseResult
--progressiveParse _ | trace "STARTING PROGRESSIVE PARSE" False = undefined
progressiveParse inputPath = do
    (filePath, fileContent) <- head . dataFiles <$> getSpecifiedContent (UnspecifiedFile $ inputPath:|[])
    case parse' nukeParser filePath fileContent of
      Right x    -> pure $ toFractured Nothing filePath x
      Left  err1 ->
        case parse' acidParser filePath fileContent of
          Right x    -> pure $ toFractured Nothing filePath x
          Left  err2 ->
            case parse' newickStreamParser filePath fileContent of
              Right x    -> pure $ toFractured Nothing filePath x
              Left  err3 ->
                case parse' verStreamParser filePath fileContent of
                  Right x    -> pure $ toFractured Nothing filePath x
                  Left  err4 ->
                    case dotParse fileContent of
                      Right x    -> pure $ toFractured Nothing filePath x
                      Left  _ ->
                        case parse' tntStreamParser filePath fileContent of
                          Right x    -> pure $ toFractured Nothing filePath x
                          Left  err5 ->
                            case parse' nexusStreamParser filePath fileContent of
                              Right x    -> ExceptT . pure . toEither . expandDynamicCharsMarkedAsAligned $ toFractured Nothing filePath x
                              Left  err6 ->
                                let previousErrors      = [(err1,"Fasta"),(err2,"Fasta"),(err3,"Newick tree"),(err4,"VER"),(err5,"Henning/TNT"),(err6,"Nexus")]
                                    (parseErr,_fileType) = maximumBy (comparing (farthestParseErr . fst)) previousErrors
                                in  throwE $ unparsable fileContent parseErr
{-
                                fail $ mconcat [ "Could not parse '"
                                               , filePath
                                               , "', appears to be a "
                                               , fileType
                                               , " file.\n"
                                               , parseErrorPretty parseErr
                                               ]
-}
  where
    -- | We use this to find the parser which got farthest through the stream before failing.
    farthestParseErr :: ParseError t e -> SourcePos
    farthestParseErr err = maximum $ errorPos err
    nukeParser = (\x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x) =<< fastaStreamParser
    acidParser = fastaStreamConverter Fasta.DNA =<< fastaStreamParser


toFractured :: (ParsedMetadata a, ParsedCharacters a, ParsedForest a) => Maybe (TCM.TCM, TCMStructure) -> FilePath -> a -> FracturedParseResult
toFractured tcmMat path =
    FPR <$> unifyCharacters
        <*> unifyMetadata
        <*> unifyGraph
        <*> const tcmMat
        <*> const path


getSpecifiedContent :: FileSpecification -> ExceptT ReadError IO FileSpecificationContent
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
      Nothing -> SpecContent (dataFiles specifiedContent) <$> getSpecifiedTcm tcm
      Just _  -> pure specifiedContent


getSpecifiedTcm :: Maybe FilePath -> ExceptT ReadError IO (Maybe (FilePath, FileContent))
getSpecifiedTcm tcmPath =
    case tcmPath of
      Nothing       -> pure Nothing
      Just tcmPath' -> do
        tcmFiles <- getFileContents tcmPath'
        case tcmFiles of
          [x] -> pure $ Just x
          []  -> throwE $ unfindable tcmPath'
          _   -> throwE $ ambiguous tcmPath' (fst <$> tcmFiles)


getSpecifiedFileContents :: Foldable f => f FilePath -> ExceptT ReadError IO [FileResult]
getSpecifiedFileContents = fmap concat . eitherTValidation . fmap getFileContents . toList


getSpecifiedContentSimple :: Foldable f => f FilePath -> ExceptT ReadError IO FileSpecificationContent
getSpecifiedContentSimple = fmap (`SpecContent` Nothing) . getSpecifiedFileContents


-- | Reads in the contents of the given FilePath, correctly interpolating glob paths
getFileContents :: FilePath -> ExceptT ReadError IO [(FilePath, FileContent)]
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
          []  -> throwE $ unfindable path
          [x] -> pure <$> readFileContent x
          xs  -> eitherTValidation $ readFileContent <$> xs
  where
    readFileContent :: FilePath -> ExceptT ReadError IO (FilePath, FileContent)
    readFileContent foundPath = do
        canRead <- liftIO $ readable <$> getPermissions foundPath
        if   not canRead
        then throwE $ unopenable foundPath
        else do
            content <- liftIO $ readFile foundPath
            pure (foundPath, content)


setCharactersToAligned :: FracturedParseResult -> FracturedParseResult
setCharactersToAligned fpr = fpr { parsedMetas = setAligned <$> parsedMetas fpr }
  where
    setAligned x = x { isDynamic = False }


expandDynamicCharsMarkedAsAligned :: FracturedParseResult -> Validation ReadError FracturedParseResult
expandDynamicCharsMarkedAsAligned fpr = updateFpr <$> result
  where
    setAligned x = x { isDynamic = False }

    updateFpr (ms, cm) = fpr
        { parsedChars = V.fromList <$> cm
        , parsedMetas = V.fromList ms
        }

    result = foldrWithKey expandDynamicChars (pure ([], [] <$ characterMap)) $ parsedMetas fpr

    characterMap = parsedChars fpr

    getRepresentativeChar = head $ toList characterMap

    expandDynamicChars
      :: Int
      -> ParsedCharacterMetadata
      -> Validation ReadError ([ParsedCharacterMetadata], Map String [ParsedCharacter])
      -> Validation ReadError ([ParsedCharacterMetadata], Map String [ParsedCharacter])
    expandDynamicChars k m acc =
        case getRepresentativeChar ! k of
          ParsedDynamicCharacter {} | not (isDynamic m) ->
            case fmap fst . sortOn snd . occurances . catMaybes $ dynCharLen . (!k) <$> toList characterMap of
              []    -> acc
              [len] -> case acc of
                         Failure _        -> acc
                         Success (ms, cm) -> pure (expandMetadata len m <> ms, expandCharacter len k <#$> cm)
              x:xs  -> const <$> acc <*> Failure (invalidPrealigned (sourceFile fpr) (x:|xs))
          _ -> prependUnmodified <$> acc
      where
        prependUnmodified (ms, cm) = (m:ms, (\i -> (((characterMap!i)!k):)) <#$> cm)

    dynCharLen (ParsedDynamicCharacter x) = length <$> x
    dynCharLen _                          = Nothing

    expandMetadata
      :: Int -- ^ Length
      -> ParsedCharacterMetadata
      -> [ParsedCharacterMetadata]
    expandMetadata len meta = replicate len $ setAligned meta

    expandCharacter
      :: Int    -- ^ Length
      -> Int    -- ^ Index
      -> String -- ^ Key
      -> v
      -> [ParsedCharacter]
    expandCharacter len i k _ =
        case (characterMap ! k) ! i of
          ParsedDynamicCharacter  Nothing  -> replicate len $ ParsedDiscreteCharacter Nothing
          ParsedDynamicCharacter (Just xs) -> toList $ ParsedDiscreteCharacter . Just <$> xs
          _                                -> error "Bad character indexing in Read.Evaluate.expandCharacter"


removeGapsFromDynamicCharsNotMarkedAsAligned :: FracturedParseResult -> FracturedParseResult
removeGapsFromDynamicCharsNotMarkedAsAligned fpr =
    fpr { parsedChars = fmap removeGapsFromUnalignedDynamicChars <$> parsedChars fpr }
  where
    removeGapsFromUnalignedDynamicChars :: ParsedCharacter -> ParsedCharacter
    removeGapsFromUnalignedDynamicChars (ParsedDynamicCharacter (Just xs)) = ParsedDynamicCharacter . NE.nonEmpty $ NE.filter (/= pure "-") xs
    removeGapsFromUnalignedDynamicChars e = e
