-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.ParseStreams
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Read.ParseStreams
  ( parseSpecifiedFile
  , removeGaps
  ) where

import           Control.Monad.Trans.Validation
import           Data.Alphabet
import           Data.Bifunctor                    (first)
import           Data.FileSource
import           Data.Foldable
import           Data.Functor
import           Data.Key
import           Data.List                         (sortOn)
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NE
import           Data.List.Utility                 (occurances)
import           Data.Map                          (Map, updateLookupWithKey)
import qualified Data.Map                          as M
import           Data.Maybe                        (mapMaybe)
import           Data.MonoTraversable
import           Data.Normalization.Character
import           Data.Normalization.Metadata
import           Data.Normalization.Topology
import           Data.Ord                          (comparing)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.TCM                          (TCMDiagnosis (..), TCMStructure (..), diagnoseTcm)
import qualified Data.TCM                          as TCM
import           Data.Unification
import           Data.Validation
import qualified Data.Vector.NonEmpty              as VNE
import           Data.Void
import           File.Format.Dot
import           File.Format.Fasta                 hiding (FastaSequenceType (..))
import qualified File.Format.Fasta                 as Fasta (FastaSequenceType (..))
import           File.Format.Fastc                 hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                 (nexusStreamParser)
import           File.Format.TNT                   hiding (weight)
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
import           PCG.Command.Read
import           PCG.Command.Read.InputStreams
import           PCG.Command.Read.ReadCommandError
import           Prelude                           hiding (readFile)
import           System.FilePath                   (takeFileName)
import           Text.Megaparsec


-- |
-- Used as a simplified binding of the parser action, with specific error handling.
parse' :: Stream s => Parsec Void s a -> FileSource -> s -> Validation ReadCommandError a
parse' parser fp = fromEither . first (unparsable fp) . runStreamParser parser fp


-- |
-- Specify the polymorphic, custom error type as 'Void'.
runStreamParser :: Parsec Void s a -> FileSource -> s -> Either (ParseErrorBundle s Void) a
runStreamParser parser fp = parse parser sourceFileName
  where
    -- |
    -- We take the "base name" and extension of the file.
    sourceFileName = takeFileName $ otoList fp


removeGaps :: Functor f => f PartialInputData -> f PartialInputData
removeGaps = fmap removeGapsFromDynamicCharactersNotMarkedAsAligned


parseSpecifiedFile :: FileSpecification -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
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
  -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
parseUnspecified (SpecContent fs) = traverse (progressiveParse . fst . dataFile) fs


transformToAligned
  :: NonEmpty PartialInputData
  -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
transformToAligned = ValidationT . pure . traverse (expandDynamicCharactersMarkedAsAligned . setCharactersToAligned)


setTcm :: TCM -> PartialInputData -> PartialInputData
setTcm t pid = pid
             { parsedMetas = fmap metadataUpdate <$> parsedMetas pid
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
        , alphabet = relatedAlphabet
        }


fastaDNA :: FileSpecification -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
fastaDNA = fastaWithValidator $
    \x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x


fastaAminoAcid :: FileSpecification -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
fastaAminoAcid = fastaWithValidator $ fastaStreamConverter Fasta.AminoAcid


fastaWithValidator
  :: (FastaParseResult -> Parsec Void FileContent TaxonSequenceMap)
  -> FileSpecification
  -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
fastaWithValidator validator spec = getSpecifiedContent spec >>= parseSpecifiedContent parse''
  where
    parse'' :: FileResult -> Validation ReadCommandError PartialInputData
    parse'' (path, content) = toFractured Nothing path <$> parseResult
      where
        parseResult = parse' combinator path content
        combinator  = validator =<< fastaStreamParser


parseSpecifiedContent
  :: (FileResult -> Validation ReadCommandError PartialInputData)
  -> FileSpecificationContent
  -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
parseSpecifiedContent parse'' (SpecContent fs) = ValidationT . pure $ traverse (parse'' . dataFile) fs


parseAndSetTCM
  :: Functor f
  => FileSource
  -> f PartialInputData
  -> ValidationT ReadCommandError IO (f PartialInputData)
parseAndSetTCM tcmPath pids = getSpecifiedTcm tcmPath >>= f
  where
    f (path, content) = ValidationT . pure $ reassignTCMs <$> parse' tcmStreamParser path content

    reassignTCMs tcmVal = setTcm tcmVal <$> pids


parseCustomAlphabet
  :: NonEmpty FileSource
  -> FileSource
  -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
parseCustomAlphabet dataFileSources tcmPath = getSpecifiedContent spec
                                          >>= parseFiles
                                          >>= parseAndSetTCM tcmPath
  where
    spec = CustomAlphabetFile dataFileSources tcmPath

    parseFiles (SpecContent fs) = ValidationT . pure $ traverse parse'' fs

    parse'' :: DataContent -> Validation ReadCommandError PartialInputData
    parse'' (DataContent (path, content) _) = fracturedResult
      where
        fracturedResult = parse' jointcombinator path content
        -- Intelligently decide which parser to try first
        jointcombinator
          | extractExtension path == Just "fastc" = try fastcCombinator <|> fastaCombinator
          | otherwise                             = try fastaCombinator <|> fastcCombinator
          where
            fastcCombinator = fmap (toFractured Nothing path) fastcStreamParser
            fastaCombinator = fmap (toFractured Nothing path) $
                              fastaStreamParser >>=
                              (\x -> try (fastaStreamConverter Fasta.DNA       x)
                                 <|> try (fastaStreamConverter Fasta.RNA       x)
                                 <|>      fastaStreamConverter Fasta.AminoAcid x)


progressiveParse :: FileSource -> ValidationT ReadCommandError IO PartialInputData
progressiveParse inputPath = do
    SpecContent (fc:|_) <- getSpecifiedContent (UnspecifiedFile $ inputPath:|[])

    let (filePath, fileContent)   = dataFile fc
        (preferredFound, parsers) = getParsersToTry filePath

    -- Use the Either Left value to short circuit on a succussful parse
    -- Otherwise collect all parse errors in the Right value
    case traverse (\f -> f filePath fileContent) parsers of
      Left  pid    -> pure pid
      Right errors -> invalid . unparsable inputPath $
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
--    :: FileSource
--    -> ( Bool
--       , NonEmpty (FileSource -> s -> Either PartialInputData (ParseError Char Void))
--       )
    getParsersToTry filePath =
        -- Attempt to extract the extension of the file to parse.
        case extractExtension filePath of
        -- If there was no extension, try the default order
          Nothing -> noPreferedParser
        -- Otherwise we attempt to interpret which parser would be best to try first.
          Just extension ->
        -- We do this by first looking up the file extension in a list of non-empty
        -- lists of aliases for the same type of file.
            case filter (elem extension) fileExtensions of
              [] -> noPreferedParser
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
        noPreferedParser = (False, NE.fromList $ toList parserMap)

        fileExtensions :: [NonEmpty FileExtension]
        fileExtensions = foldMapWithKey (\k v -> [k :| snd v]) associationMap

--      parserMap :: Map String (FileSource -> s -> Either PartialInputData (ParseError Char Void))
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
          :: ( HasNormalizedMetadata a
             , HasNormalizedCharacters a
             , HasNormalizedTopology a
             )
          => Parsec Void s a
          -> FileSource
          -> s
          -> Either PartialInputData (ParseErrorBundle s Void)
        makeParser parser path = eSwap . fmap (toFractured Nothing path) . runStreamParser parser path
          where
            eSwap (Left  x) = Right x
            eSwap (Right x) = Left  x

        nukeParser = (\x -> try (fastaStreamConverter Fasta.DNA x)
                            <|>  fastaStreamConverter Fasta.RNA x
                     ) =<< fastaStreamParser


toFractured
  :: ( HasNormalizedMetadata a
     , HasNormalizedCharacters a
     , HasNormalizedTopology a
     )
  => Maybe (TCM.TCM, TCMStructure)
  -> FileSource
  -> a
  -> PartialInputData
toFractured tcmMat path =
    PID <$> getNormalizedCharacters
        <*> getNormalizedMetadata
        <*> getNormalizedTopology
        <*> const tcmMat
        <*> const path


setCharactersToAligned :: PartialInputData -> PartialInputData
setCharactersToAligned pid = pid { parsedMetas = fmap setAligned <$> parsedMetas pid }
  where
    setAligned x = x { isDynamic = False }


expandDynamicCharactersMarkedAsAligned :: PartialInputData -> Validation ReadCommandError PartialInputData
expandDynamicCharactersMarkedAsAligned pid =
    case parsedMetas pid of
      Nothing -> pure pid
      Just xs -> updatePid . foldl1 joinExpandedSeqs <$> traverseWithKey1 expandDynamicCharacters xs
  where
    setAligned x = x { isDynamic = False }

    updatePid (ms, cm) = pid
        { parsedChars = VNE.fromNonEmpty <$> cm
        , parsedMetas = Just $ VNE.fromNonEmpty ms
        }

    joinExpandedSeqs (ms1, cm1) (ms2, cm2) = (ms1 <> ms2, M.intersectionWith (<>) cm1 cm2)

                 -- Map Identifier ParsedChars
    characterMap :: NormalizedCharacters
    characterMap = parsedChars pid

    getRepresentativeChar = head $ toList characterMap

    expandDynamicCharacters
      :: Int
      -> NormalizedMetadata
      -> Validation ReadCommandError (NonEmpty NormalizedMetadata, Map Identifier (NonEmpty NormalizedCharacter))
    expandDynamicCharacters k m =
        case getRepresentativeChar ! k of
          NormalizedDynamicCharacter {} | not (isDynamic m) ->
            case getDynamicCharacterLengths singleCharacterMap of
              []    -> Failure $ unaligned (sourceFile pid) (0:|([] :: [Word]))
              [len] -> pure (expandMetadata len m, expandCharacter len <$> singleCharacterMap)
              x:xs  -> Failure $ unaligned (sourceFile pid) (x:|xs)
          _ -> pure (pure m, pure <$> singleCharacterMap)
      where
        singleCharacterMap = (!k) <$> characterMap

    -- Get the lengths of all the dynamic characters in the map.
    -- They should all be the same length, returning a singleton list.
    getDynamicCharacterLengths :: Foldable f => f NormalizedCharacter -> [Int]
    getDynamicCharacterLengths = fmap fst . sortOn snd . occurances . mapMaybe dynCharLen . toList

    dynCharLen (NormalizedDynamicCharacter x) = length <$> x
    dynCharLen _                              = Nothing

    expandMetadata
      :: Int -- ^ Length
      -> NormalizedMetadata
      -> NonEmpty NormalizedMetadata
    expandMetadata len meta = let m = setAligned meta
                              in  m :| replicate (len-1) m

    expandCharacter
      :: Int          -- ^ Length
      -> NormalizedCharacter
      -> NonEmpty NormalizedCharacter
    expandCharacter len v =
        case v of
          NormalizedDynamicCharacter  Nothing  -> let miss = NormalizedDiscreteCharacter Nothing
                                                  in  miss :| replicate (len-1) miss
          NormalizedDynamicCharacter (Just xs) -> toNonEmpty $ NormalizedDiscreteCharacter . Just <$> xs
          _                                    -> error "Bad character indexing in Read.Evaluate.expandCharacter"


removeGapsFromDynamicCharactersNotMarkedAsAligned :: PartialInputData -> PartialInputData
removeGapsFromDynamicCharactersNotMarkedAsAligned pid =
    pid { parsedChars = fmap removeGapsFromUnalignedDynamicCharacters <$> parsedChars pid }
  where
    removeGapsFromUnalignedDynamicCharacters :: NormalizedCharacter -> NormalizedCharacter
    removeGapsFromUnalignedDynamicCharacters (NormalizedDynamicCharacter (Just xs)) = NormalizedDynamicCharacter . NE.nonEmpty $ NE.filter (/= pure "-") xs
    removeGapsFromUnalignedDynamicCharacters e = e
