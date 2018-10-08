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
import           Data.Char                                 (toLower)
import           Data.Compact                              (compact)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Functor
import           Data.Key
import           Data.List                                 (sortOn)
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NE
import           Data.List.Utility                         (occurances)
import           Data.Map                                  (Map, updateLookupWithKey)
import qualified Data.Map                                  as M
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
import           Prelude                                   hiding (readFile)
import           System.Directory
import           System.FilePath                           (takeExtension)
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
removeGaps = fmap removeGapsFromDynamicCharactersNotMarkedAsAligned


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
    ExceptT . pure . toEither . sequenceA $ expandDynamicCharactersMarkedAsAligned . setCharactersToAligned <$> combined



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
fastaDNA = fastaWithValidator $
    \x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x


fastaAminoAcid :: FileSpecification -> ExceptT ReadError IO [FracturedParseResult]
fastaAminoAcid = fastaWithValidator $ fastaStreamConverter Fasta.AminoAcid


fastaWithValidator
  :: (FastaParseResult -> Parsec Void FileContent TaxonSequenceMap)
  -> FileSpecification
  -> ExceptT ReadError IO [FracturedParseResult]
fastaWithValidator validator spec = getSpecifiedContent spec >>= (ExceptT . pure . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first (unparsable content) $ parse' combinator path content
        combinator  = validator =<< fastaStreamParser



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


progressiveParse :: FilePath -> ExceptT ReadError IO FracturedParseResult
progressiveParse inputPath = do
    (filePath, fileContent) <- head . dataFiles <$> getSpecifiedContent (UnspecifiedFile $ inputPath:|[])
    let parsers = getParsersToTry $ takeExtension filePath
    -- Use the Either Left value to short circuit on a succussful parse
    -- Otherwise collect all parse errors in the Right value
    case traverse (\f -> f filePath fileContent) parsers of
      Left  fpr    -> pure fpr
      Right errors -> let parseErr = maximumBy (comparing farthestParseErr) errors
                      in  throwE $ unparsable fileContent parseErr
  where
    -- |
    -- We use this to find the parser which got farthest through the stream before failing.
    farthestParseErr :: ParseError t e -> SourcePos
    farthestParseErr err = maximum $ errorPos err

    -- |
    -- Takes a file extension and returns the /ordered/ list of parsers to try.
    -- Attempts to place the parser that is associated with the given file
    -- extension as the first parser to try.
    --
    -- Makes our parsing phase marginally more efficient.
--  getParsersToTry :: String -> [FilePath -> s -> Either FracturedParseResult (ParseError Char Void)]
    getParsersToTry ext =
        -- We do this by first looking up the file extension in a list of non-empty
        -- lists of aliases for the same type of file.
        case filter (elem extension) fileExtensions of
          []       -> toList parserMap
        -- If we find a non-empty list of file extensions that contains the given
        -- file extension, then we take the head of the non-empty list and use this
        -- as our representative file extension key.
          (k:|_):_ ->
        -- Lastly we lookup and remove the representative file extension key from a
        -- map of file parsers.
            case updateLookupWithKey (const (const Nothing)) k parserMap of
        -- This case should never be entered, but it is sensibly covered.
              (Nothing, m) ->     toList m
        -- This returns the parser associated with the representative file extension
        -- key and the map of file parsers with the queried parser removed. With
        -- these elements in scope, we simply place the desired parser at the head of
        -- the list of all the parsers.
              (Just  p, m) -> p : toList m
      where
        -- Convert the extension to lower case for simpler string comparisons
        extension = toLower <$> ext

        fileExtensions :: [NonEmpty String]
        fileExtensions = fmap ('.':) <$> foldMapWithKey (\k v -> [k :| snd v]) associationMap

--      parserMap :: Map String (FilePath -> s -> Either FracturedParseResult (ParseError Char Void))
        parserMap = fst <$> associationMap

        associationMap = M.fromList
            [ ("fas", (makeParser         nukeParser, ["fast","fasta"]))
            , ("tre", (makeParser newickStreamParser, ["tree","new","newick"]))
            , ("ver", (makeParser    verStreamParser, []))
            , ("tnt", (makeParser    tntStreamParser, ["hen","hennig"]))
            , ("nex", (makeParser  nexusStreamParser, ["nexus"]))
            ]

        makeParser
          :: ( ParsedMetadata a
             , ParsedCharacters a
             , ParsedForest a
             , Token s ~ Char
             )
          => Parsec Void s a
          -> FilePath
          -> s
          -> Either FracturedParseResult (ParseError (Token s) Void)
        makeParser parser path = eSwap . fmap (toFractured Nothing path) . parse' parser path
          where
            eSwap (Left  x) = Right x
            eSwap (Right x) = Left  x

        nukeParser = (\x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x) =<< fastaStreamParser


toFractured
  :: ( ParsedMetadata a
     , ParsedCharacters a
     , ParsedForest a
     )
  => Maybe (TCM.TCM, TCMStructure)
  -> FilePath
  -> a
  -> FracturedParseResult
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


expandDynamicCharactersMarkedAsAligned :: FracturedParseResult -> Validation ReadError FracturedParseResult
expandDynamicCharactersMarkedAsAligned fpr = updateFpr <$> result
  where
    setAligned x = x { isDynamic = False }

    updateFpr (ms, cm) = fpr
        { parsedChars = V.fromList <$> cm
        , parsedMetas = V.fromList ms
        }

    result = foldrWithKey expandDynamicCharacters (pure ([], [] <$ characterMap)) $ parsedMetas fpr

    characterMap = parsedChars fpr

    getRepresentativeChar = head $ toList characterMap

    expandDynamicCharacters
      :: Int
      -> ParsedCharacterMetadata
      -> Validation ReadError ([ParsedCharacterMetadata], Map String [ParsedCharacter])
      -> Validation ReadError ([ParsedCharacterMetadata], Map String [ParsedCharacter])
    expandDynamicCharacters k m acc =
        case getRepresentativeChar ! k of
          ParsedDynamicCharacteracter {} | not (isDynamic m) ->
            case fmap fst . sortOn snd . occurances . catMaybes $ dynCharLen . (!k) <$> toList characterMap of
              []    -> acc
              [len] -> case acc of
                         Failure _        -> acc
                         Success (ms, cm) -> pure (expandMetadata len m <> ms, expandCharacter len k <#$> cm)
              x:xs  -> const <$> acc <*> Failure (invalidPrealigned (sourceFile fpr) (x:|xs))
          _ -> prependUnmodified <$> acc
      where
        prependUnmodified (ms, cm) = (m:ms, (\i -> (((characterMap!i)!k):)) <#$> cm)

    dynCharLen (ParsedDynamicCharacteracter x) = length <$> x
    dynCharLen _                               = Nothing

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
          ParsedDynamicCharacteracter  Nothing  -> replicate len $ ParsedDiscreteCharacter Nothing
          ParsedDynamicCharacteracter (Just xs) -> toList $ ParsedDiscreteCharacter . Just <$> xs
          _                                     -> error "Bad character indexing in Read.Evaluate.expandCharacter"


removeGapsFromDynamicCharactersNotMarkedAsAligned :: FracturedParseResult -> FracturedParseResult
removeGapsFromDynamicCharactersNotMarkedAsAligned fpr =
    fpr { parsedChars = fmap removeGapsFromUnalignedDynamicCharacters <$> parsedChars fpr }
  where
    removeGapsFromUnalignedDynamicCharacters :: ParsedCharacter -> ParsedCharacter
    removeGapsFromUnalignedDynamicCharacters (ParsedDynamicCharacteracter (Just xs)) = ParsedDynamicCharacteracter . NE.nonEmpty $ NE.filter (/= pure "-") xs
    removeGapsFromUnalignedDynamicCharacters e = e
