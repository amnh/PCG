{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Command.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Character.Parsed
import           Bio.Metadata.Parsed
import           Bio.Graph
import           Bio.Graph.Forest.Parsed
import           Control.Evaluation
import           Control.Monad                (liftM2, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Parallel.Strategies
import           Control.Parallel.Custom
import           Data.Alphabet   --    hiding (AmbiguityGroup)
-- import           Data.Alphabet.IUPAC
import           Data.Bifunctor               (bimap,first)
-- import           Data.Char                    (isLower,toLower,isUpper,toUpper)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Functor
--import           Data.Key
--import           Data.List                    (intercalate)
import           Data.List.NonEmpty           (NonEmpty(..))
-- import qualified Data.List.NonEmpty    as NE
-- import           Data.List.Utility            (subsetOf)
-- import           Data.Map                     (Map,assocs,insert,union)
-- import qualified Data.Map              as M
-- import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (comparing)
import           Data.Semigroup
import           Data.TCM                     (TCMDiagnosis(..), TCMStructure(..), diagnoseTcm)
import qualified Data.TCM              as TCM
import           Data.Text.IO                 (readFile)
-- import           Data.Vector                  (Vector)
-- import qualified Data.Vector           as V   (zipWith)
import           Data.Void
import           File.Format.Dot
import           File.Format.Fasta   hiding   (FastaSequenceType(..))
import qualified File.Format.Fasta   as Fasta (FastaSequenceType(..))
import           File.Format.Fastc   hiding   (Identifier)
import           File.Format.Newick
import           File.Format.Nexus            (nexusStreamParser)
import           File.Format.TNT     hiding   (weight)
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
import           PCG.Syntax                   (Command(..))
import           PCG.Command.Read
import           PCG.Command.Read.DecorationInitialization
import           PCG.Command.Read.ReadError
import           PCG.Command.Read.Unification.Master
import           Prelude             hiding   (lookup, readFile)
import           System.Directory
import           System.FilePath.Glob
import           Text.Megaparsec

--import Debug.Trace

-- type SearchState = EvaluationT IO (Either TopologicalResult CharacterResult)


parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
parse' = parse


--evaluate :: Command -> EvaluationT IO a -> EvaluationT IO (Either TopologicalResult DecoratedCharacterResult)
--evaluate :: Command -> EvaluationT IO a -> EvaluationT IO (Either TopologicalResult CharacterResult)
evaluate :: Command -> EvaluationT IO a -> SearchState -- EvaluationT IO (Either TopologicalResult CharacterResult)
-- evaluate (READ fileSpecs) _old | trace ("Evaluated called: " <> show fileSpecs) False = undefined
-- evaluate (READ fileSpecs) _old | trace "STARTING READ COMMAND" False = undefined
evaluate (READ (ReadCommand fileSpecs)) _old = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runEitherT . eitherTValidation $ parmap rpar parseSpecifiedFile fileSpecs
    case result of
      Left pErr -> fail $ show pErr   -- Report structural errors here.
      Right xs ->
        case decoration . masterUnify $ transformation <$> concat xs of
--        case masterUnify $ transformation <$> concat xs of
          Left uErr -> fail $ show uErr -- Report unification errors here.
           -- TODO: rectify against 'old' SearchState, don't just blindly merge or ignore old state
          Right g   -> pure g
                       -- (liftIO . putStrLn {- . take 500000 -} $ either show (ppTopElement . toXML) g)
                       -- (liftIO . putStrLn $ renderSequenceCosts g)
                       -- (liftIO . putStrLn $ show g) $> g
  where
    transformation = id -- expandIUPAC
    decoration     = fmap (fmap initializeDecorations2)

evaluate _ _ = fail "Invalid READ command binding"

{-
renderSequenceCosts :: Either t (PhylogeneticSolution (PhylogeneticDAG2 e n u v w x y z)) -> String
renderSequenceCosts (Left    _) = "<Trees only>"
renderSequenceCosts (Right sol) = outputStream
  where
    outputStream = foldMapWithKey f $ phylogeneticForests sol
    f key forest = unlines
        [ "Solution #" <> show key
        , ""
        , indent $ foldMapWithKey g forest
        ]
    g key dags = unlines
        [ "Forest #" <> show key
        , ""
        , indent $ renderSummary dags
        ]
    indent = intercalate "\n" . fmap ("  "<>) . lines
--    unlines . toList . fmap (unlines . toList . fmap (unlines . fmap show . toList . rootCosts)) . phylogeneticForests
-}


parseSpecifiedFile  :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
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
    fmap (fmap setCharactersToAligned) $
      case tcmContent of
        Nothing              -> pure subContent
        Just (path, content) -> do
          tcmMat <- hoistEither . first (unparsable content) $ parse' tcmStreamParser path content
          traverse (hoistEither . setTcm tcmMat path) subContent


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


fastaDNA :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
--fastaDNA spec | trace ("fasta DNA parser with spec " ++ show spec) False = undefined
fastaDNA spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = {- (\x -> trace (show x) x) . -} first (unparsable content) $ parse' combinator path content
        combinator  = (\x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x) =<< fastaStreamParser


-- TODO: abstract these two (three) v^
fastaAminoAcid :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
fastaAminoAcid spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent parse'')
  where
    parse'' :: FileResult -> Either ReadError FracturedParseResult
    parse'' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first (unparsable content) $ parse' combinator path content
        combinator  = fastaStreamConverter Fasta.AminoAcid =<< fastaStreamParser


parseSpecifiedContent :: (FileResult -> Either ReadError FracturedParseResult) -> FileSpecificationContent -> Either ReadError [FracturedParseResult]
parseSpecifiedContent parse'' = eitherValidation . fmap parse'' . dataFiles


parseCustomAlphabet :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseCustomAlphabet spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContentWithTcm)
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
                          (\x -> try (fastaStreamConverter Fasta.DNA  x)
                             <|> try (fastaStreamConverter Fasta.RNA  x)
                             <|> fastaStreamConverter Fasta.AminoAcid x)

    parseSpecifiedContentWithTcm :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContentWithTcm specContent = do
        tcmMay <-
          case tcmFile specContent of
            Nothing              -> pure Nothing
            Just (path, content) -> bimap (unparsable content) Just $ parse' tcmStreamParser path content
        eitherValidation . fmap (parse'' tcmMay) $ dataFiles specContent

{-
applyReferencedTCM :: FracturedParseResult -> FracturedParseResult
applyReferencedTCM fpr =
    case relatedTcm fpr of
       Nothing -> fpr
       Just x  ->
         let newAlphabet = fromSymbols $ customAlphabet x
             newTcm      = transitionCosts x
         in  fpr
             { parsedMetas = updateAlphabet newAlphabet <$> parsedMetas fpr
            }
  where
    updateAlphabet parsedMeta newValue = parsedMeta { alphabet = newValue }
-}

--prependFilenamesToCharacterNames :: FracturedParseResult -> FracturedParseResult
--prependFilenamesToCharacterNames fpr = fpr { parsedMetas = prependName (sourceFile fpr) <$> parsedMetas fpr }


setCharactersToAligned :: FracturedParseResult -> FracturedParseResult
setCharactersToAligned fpr = fpr { parsedMetas = setAligned <$> parsedMetas fpr }
  where
    setAligned x = x { isDynamic = False }

{- removed to eliminate compilation warning
expandIUPAC :: FracturedParseResult -> FracturedParseResult
expandIUPAC fpr = fpr { parsedChars = newTreeChars }
  where
    newTreeChars = f (parsedChars fpr) (parsedMetas fpr)
    f :: TreeChars -> Vector ParsedCharacterMetadata -> TreeChars
    f mapping meta = g <$> mapping
      where
        g :: ParsedChars -> ParsedChars
        g = V.zipWith h meta
          where
            h :: ParsedCharacterMetadata -> Maybe ParsedChar -> Maybe ParsedChar
            h cInfo seqMay = expandCodes <$> seqMay
              where
                cAlph = alphabet cInfo

                expandCodes :: ParsedChar -> ParsedChar
                expandCodes x
                  | isAlphabetDna       cAlph = expandOrId nucleotideIUPAC <$> x
                  | isAlphabetAminoAcid cAlph = expandOrId aminoAcidIUPAC  <$> x
                  | otherwise = x
    expandOrId m x = fromMaybe x $ x `lookup` m
-}

-- TODO: check file extension, to guess which parser to use first
progressiveParse :: FilePath -> EitherT ReadError IO FracturedParseResult
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
                              Right x    -> pure $ toFractured Nothing filePath x
                              Left  err6 ->
                                let previousErrors      = [(err1,"Fasta"),(err2,"Fasta"),(err3,"Newick tree"),(err4,"VER"),(err5,"Henning/TNT"),(err6,"Nexus")]
                                    (parseErr,_fileType) = maximumBy (comparing (farthestParseErr . fst)) previousErrors
                                in  left $ unparsable fileContent parseErr
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
{--}

{- removed to eliminate compilation warning
nucleotideIUPAC :: Map (AmbiguityGroup String) (AmbiguityGroup String)
nucleotideIUPAC = casei $ nonEmptyMap core
  where
    ref  = (core !)
    core = M.fromList
         [ (["A"], ["A"]     )
         , (["G"], ["G"]     )
         , (["C"], ["C"]     )
         , (["T"], ["T"]     )
         , (["-"], ["-"]     ) -- assume 5th state (TODO: fix this)
         , (["U"], ref ["T"] )
         , (["R"], ref ["A"] <> ref ["G"])
         , (["M"], ref ["A"] <> ref ["C"])
         , (["W"], ref ["A"] <> ref ["T"])
         , (["S"], ref ["G"] <> ref ["C"])
         , (["K"], ref ["G"] <> ref ["T"])
         , (["Y"], ref ["C"] <> ref ["T"])
         , (["V"], ref ["A"] <> ref ["G"] <> ref ["C"])
         , (["D"], ref ["A"] <> ref ["G"] <> ref ["T"])
         , (["H"], ref ["A"] <> ref ["C"] <> ref ["T"])
         , (["B"], ref ["G"] <> ref ["C"] <> ref ["T"])
         , (["N"], ref ["A"] <> ref ["G"] <> ref ["C"] <> ref ["T"])
         , (["X"], ref ["N"])
         , (["?"], ref ["A"] <> ref ["G"] <> ref ["C"] <> ref ["T"] <> ref ["-"])
         ]


aminoAcidIUPAC :: Map (AmbiguityGroup String) (AmbiguityGroup String)
aminoAcidIUPAC = casei . nonEmptyMap $ core `union` multi
  where
    core         = M.fromList $ zip symbolGroups symbolGroups
    symbolGroups = pure . pure <$> "ACDEFGHIKLMNPQRSTVWY-"
    ref          = (core !)
    allSymbols   = foldl1 (<>) symbolGroups
    allAcids     = foldl1 (<>) $ init symbolGroups
    multi        = M.fromList
                 [ (["B"], ref ["D"] <> ref ["N"])
                 , (["Z"], ref ["E"] <> ref ["Q"])
                 , (["X"], allAcids  )
                 , (["?"], allSymbols)
                 ]

nonEmptyMap :: Map [a] [a] -> Map (NonEmpty a) (NonEmpty a)
nonEmptyMap = fmap NE.fromList . M.mapKeysMonotonic NE.fromList

casei :: Map (AmbiguityGroup String ) v -> Map (AmbiguityGroup String) v
casei x = foldl f x $ assocs x
  where
    f m ([k]:|[], v)
      | isLower k  = insert (pure . pure $ toUpper k) v m
      | isUpper k  = insert (pure . pure $ toLower k) v m
      | otherwise  = m
    f m (_    , _) = m
-}


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
      Nothing -> SpecContent (dataFiles specifiedContent) <$> getSpecifiedTcm tcm
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


getSpecifiedFileContents :: Foldable f => f FilePath -> EitherT ReadError IO [FileResult]
getSpecifiedFileContents = fmap concat . eitherTValidation . fmap getFileContents . toList


getSpecifiedContentSimple :: Foldable f => f FilePath -> EitherT ReadError IO FileSpecificationContent
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
