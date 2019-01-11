{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Character.Parsed
import           Bio.Graph
import           Bio.Graph.Forest.Parsed
import           Bio.Metadata
import           Bio.Metadata.Parsed
import           Control.Monad                             (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Alphabet
import           Data.Bifunctor                            (first)
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
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.String                               (IsString (fromString))
import           Data.TCM                                  (TCMDiagnosis (..), TCMStructure (..), diagnoseTcm)
import qualified Data.TCM                                  as TCM
import           Data.Text.IO                              (readFile)
import qualified Data.Text.Short                           as TS (ShortText, filter)
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


parse' :: Parsec Void s a -> String -> s -> Either (ParseErrorBundle s Void) a
parse' = parse


evaluate :: ReadCommand -> SearchState
evaluate (ReadCommand fileSpecs) = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runExceptT . eitherTValidation $ parmap rpar (fmap removeGaps . parseSpecifiedFile) fileSpecs
--    liftIO $ print result
    case result of
      Left pErr -> fail $ show pErr   -- Report structural errors here.
      Right xs ->
        case decoration . masterUnify $ transformation <$> sconcat xs of
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


parseSpecifiedFile :: FileSpecification -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
parseSpecifiedFile      AnnotatedFile          {} = fail "Annotated file specification is not implemented"
parseSpecifiedFile      ChromosomeFile         {} = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      GenomeFile             {} = fail "Genome file specification is not implemented"
parseSpecifiedFile spec@AminoAcidFile          {} = fastaAminoAcid spec
parseSpecifiedFile spec@NucleotideFile         {} = fastaDNA       spec
parseSpecifiedFile      (CustomAlphabetFile fs m) = parseCustomAlphabet fs m
parseSpecifiedFile spec@(UnspecifiedFile     _  ) = getSpecifiedContent spec >>= parseUnspecified
parseSpecifiedFile      (PrealignedFile      x  ) = parseSpecifiedFile x     >>= transformToAligned
parseSpecifiedFile      (WithSpecifiedTCM    x m) = parseSpecifiedFile x     >>= parseAndSetTCM m

parseUnspecified
  :: FileSpecificationContent
  -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
parseUnspecified (SpecContent fs) = eitherTValidation $ progressiveParse . fst . dataFile <$> fs


transformToAligned
  :: NonEmpty FracturedParseResult
  -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
transformToAligned =
  ExceptT . pure . toEither . traverse (expandDynamicCharactersMarkedAsAligned . setCharactersToAligned)


setTcm :: TCM -> FracturedParseResult -> FracturedParseResult
setTcm t  fpr = fpr
              { parsedMetas = metadataUpdate <$> parsedMetas fpr
              , relatedTcm  = Just (resultTCM, structure)
              }
  where
    relatedAlphabet                     = fromSymbols           $ customAlphabet  t
    (unfactoredWeight, unfactoredTCM)   = TCM.fromList . toList $ transitionCosts t
    (coefficient, resultTCM, structure) = (,,) <$> factoredWeight
                                               <*> factoredTcm
                                               <*> tcmStructure
                                               $   diagnoseTcm unfactoredTCM
    metadataUpdate x = x
        { weight   = weight x * fromRational unfactoredWeight * fromIntegral coefficient
        , alphabet = fmap fromString relatedAlphabet
        }


fastaDNA :: FileSpecification -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
fastaDNA = fastaWithValidator $
    \x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x


fastaAminoAcid :: FileSpecification -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
fastaAminoAcid = fastaWithValidator $ fastaStreamConverter Fasta.AminoAcid


fastaWithValidator
  :: (FastaParseResult -> Parsec Void FileContent TaxonSequenceMap)
  -> FileSpecification
  -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
fastaWithValidator validator spec = getSpecifiedContent spec >>= (ExceptT . pure . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path, content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first unparsable $ parse' combinator path content
        combinator  = validator =<< fastaStreamParser


parseSpecifiedContent
  :: (FileResult -> Either ReadError FracturedParseResult)
  -> FileSpecificationContent
  -> Either ReadError (NonEmpty FracturedParseResult)
parseSpecifiedContent parse'' (SpecContent fs) = eitherValidation $ parse'' . dataFile <$> fs


parseAndSetTCM
  :: Functor f
  => FilePath
  -> f FracturedParseResult
  -> ExceptT ReadError IO (f FracturedParseResult)
parseAndSetTCM tcmPath fprs = do
    (path, content) <- getSpecifiedTcm tcmPath
    tcmVal <- ExceptT . pure
            . first unparsable
            $ parse' tcmStreamParser path content
    pure $ setTcm tcmVal <$> fprs


parseCustomAlphabet
  :: NonEmpty FilePath
  -> FilePath
  -> ExceptT ReadError IO (NonEmpty FracturedParseResult)
parseCustomAlphabet dataFilePaths tcmPath = getSpecifiedContent spec
                                        >>= parseFiles
                                        >>= parseAndSetTCM tcmPath
  where
    spec = CustomAlphabetFile dataFilePaths tcmPath
    parseFiles (SpecContent fs) = eitherTValidation $ ExceptT . pure . parse'' <$> fs

    parse'' :: DataContent -> Either ReadError FracturedParseResult
    parse'' (DataContent (path, content) _) = fracturedResult
      where
        fracturedResult = first unparsable
                        $ parse' (try fastaCombinator <|> fastcCombinator) path content
        fastcCombinator = fmap (toFractured Nothing path) fastcStreamParser
        fastaCombinator = fmap (toFractured Nothing path) $
                          fastaStreamParser >>=
                          (\x -> try (fastaStreamConverter Fasta.DNA       x)
                             <|> try (fastaStreamConverter Fasta.RNA       x)
                             <|>      fastaStreamConverter Fasta.AminoAcid x)


progressiveParse :: FilePath -> ExceptT ReadError IO FracturedParseResult
progressiveParse inputPath = do
    SpecContent (fc:|_) <- getSpecifiedContent (UnspecifiedFile $ inputPath:|[])

    let (filePath, fileContent)   = dataFile fc
        (preferredFound, parsers) = getParsersToTry $ takeExtension filePath

    -- Use the Either Left value to short circuit on a succussful parse
    -- Otherwise collect all parse errors in the Right value
    case traverse (\f -> f filePath fileContent) parsers of
      Left  fpr    -> pure fpr
      Right errors -> throwE . unparsable $
                        if   preferredFound
                        then NE.head errors
                        else maximumBy (comparing farthestParseErr) errors
  where
    -- |
    -- We use this to find the parser which got farthest through the stream
    -- before failing, but only when we didn't find a preferred parser to try
    -- first.
    farthestParseErr :: ParseErrorBundle s e -> Int
    farthestParseErr = pstateOffset . bundlePosState

    -- |
    -- Takes a file extension and returns the /ordered/ list of parsers to try.
    -- Attempts to place the parser that is associated with the given file
    -- extension as the first parser to try.
    --
    -- Makes our parsing phase marginally more efficient.
--  getParsersToTry
--    :: String
--    -> ( Bool
--       , NonEmpty (FilePath -> s -> Either FracturedParseResult (ParseError Char Void))
--       )
    getParsersToTry ext =
        -- We do this by first looking up the file extension in a list of non-empty
        -- lists of aliases for the same type of file.
        case filter (elem extension) fileExtensions of
          []       -> (False, NE.fromList $ toList parserMap)
        -- If we find a non-empty list of file extensions that contains the given
        -- file extension, then we take the head of the non-empty list and use this
        -- as our representative file extension key.
          (k:|_):_ ->
        -- Lastly we lookup and remove the representative file extension key from a
        -- map of file parsers.
            case  updateLookupWithKey (const (const Nothing)) k parserMap of
        -- This case should never be entered, but it is sensibly covered.
              (Nothing, m) -> (False, NE.fromList $ toList m)
        -- This returns the parser associated with the representative file extension
        -- key and the map of file parsers with the queried parser removed. With
        -- these elements in scope, we simply place the desired parser at the head of
        -- the list of all the parsers.
              (Just  p, m) -> ( True, p :| toList m)
      where
        -- Convert the extension to lower case for simpler string comparisons
        -- Also romove the leading '.' if it exists
        extension = dropDot $ toLower <$> ext
          where
            dropDot ('.':xs) = xs
            dropDot      xs  = xs

        fileExtensions :: [NonEmpty String]
        fileExtensions = foldMapWithKey (\k v -> [k :| snd v]) associationMap

--      parserMap :: Map String (FilePath -> s -> Either FracturedParseResult (ParseError Char Void))
        parserMap = fst <$> associationMap

        associationMap = M.fromList
            [ ("fas", (makeParser         nukeParser, ["fast","fasta"]))
            , ("tre", (makeParser newickStreamParser, ["tree","new","newick","enew","enewick"]))
            , ("dot", (makeParser    dotStreamParser, []))
            , ("ver", (makeParser    verStreamParser, []))
            , ("tnt", (makeParser    tntStreamParser, ["hen","hennig","ss"]))
            , ("nex", (makeParser  nexusStreamParser, ["nexus"]))
            ]

        makeParser
          :: ( ParsedMetadata a
             , ParsedCharacters a
             , ParsedForest a
             )
          => Parsec Void s a
          -> FilePath
          -> s
          -> Either FracturedParseResult (ParseErrorBundle s Void)
        makeParser parser path = eSwap . fmap (toFractured Nothing path) . parse' parser path
          where
            eSwap (Left  x) = Right x
            eSwap (Right x) = Left  x

        nukeParser = (\x -> try (fastaStreamConverter Fasta.DNA x)
                            <|>  fastaStreamConverter Fasta.RNA x
                     ) =<< fastaStreamParser


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
getSpecifiedContent (UnspecifiedFile    xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (AminoAcidFile      xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (NucleotideFile     xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (AnnotatedFile      xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (ChromosomeFile     xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (GenomeFile         xs    ) = getSpecifiedContentSimple xs
getSpecifiedContent (PrealignedFile     fs    ) = getSpecifiedContent fs
getSpecifiedContent (WithSpecifiedTCM   fs tcm) = do
    SpecContent fs' <- getSpecifiedContent fs
    tcm' <- getSpecifiedTcm tcm
    pure . SpecContent $ (DataContent <$> dataFile <*> const (Just tcm')) <$> fs'

getSpecifiedContent (CustomAlphabetFile xs tcm) = do
    xs'  <- getSpecifiedFileContents xs
    tcm' <- getSpecifiedTcm tcm
    pure . SpecContent $ (`DataContent` Just tcm') <$> xs'


getSpecifiedTcm :: FilePath -> ExceptT ReadError IO (FilePath, FileContent)
getSpecifiedTcm tcmPath = do
    tcmFiles <- getFileContents tcmPath
    case tcmFiles of
      x:|[] -> pure x
      xs    -> throwE . ambiguous tcmPath $ fst <$> xs


getSpecifiedFileContents :: Foldable1 f => f FilePath -> ExceptT ReadError IO (NonEmpty FileResult)
getSpecifiedFileContents = fmap sconcat . eitherTValidation . fmap getFileContents . toNonEmpty


getSpecifiedContentSimple :: Foldable1 f => f FilePath -> ExceptT ReadError IO FileSpecificationContent
getSpecifiedContentSimple = fmap (SpecContent . fmap (`DataContent` Nothing)) . getSpecifiedFileContents


-- | Reads in the contents of the given FilePath, correctly interpolating glob paths
getFileContents :: FilePath -> ExceptT ReadError IO (NonEmpty (FilePath, FileContent))
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
          []   -> throwE $ unfindable path
          [x]  -> pure <$> readFileContent x
          x:xs -> eitherTValidation $ readFileContent <$> x:|xs
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

                 -- Map Identifier ParsedChars
    characterMap :: TaxonCharacters
    characterMap = parsedChars fpr

    getRepresentativeChar = head $ toList characterMap

    expandDynamicCharacters
      :: Int
      -> ParsedCharacterMetadata
      -> Validation ReadError ([ParsedCharacterMetadata], Map Identifier [ParsedCharacter])
      -> Validation ReadError ([ParsedCharacterMetadata], Map Identifier [ParsedCharacter])
    expandDynamicCharacters k m acc =
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
      :: Int          -- ^ Length
      -> Int          -- ^ Index
      -> TS.ShortText -- ^ Key
      -> v
      -> [ParsedCharacter]
    expandCharacter len i k _ =
        case (characterMap ! k) ! i of
          ParsedDynamicCharacter  Nothing  -> replicate len $ ParsedDiscreteCharacter Nothing
          ParsedDynamicCharacter (Just xs) -> toList $ ParsedDiscreteCharacter . Just <$> xs
          _                                -> error "Bad character indexing in Read.Evaluate.expandCharacter"


removeGapsFromDynamicCharactersNotMarkedAsAligned :: FracturedParseResult -> FracturedParseResult
removeGapsFromDynamicCharactersNotMarkedAsAligned fpr =
    fpr { parsedChars = fmap removeGapsFromUnalignedDynamicCharacters <$> parsedChars fpr }
  where
    removeGapsFromUnalignedDynamicCharacters :: ParsedCharacter -> ParsedCharacter
    removeGapsFromUnalignedDynamicCharacters (ParsedDynamicCharacter (Just xs)) = ParsedDynamicCharacter . NE.nonEmpty $ NE.filter (/= pure "-") xs
    removeGapsFromUnalignedDynamicCharacters e = e
