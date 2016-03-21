{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph.Parsed
import           Bio.Metadata.Class
import           Bio.Sequence.Parsed.Class
import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Evaluation
import           Data.Bifunctor             (bimap,first)
import           Data.Either.Custom
import           Data.Foldable
--import           Data.Hashable
--import           Data.HashMap.Strict        (HashMap)
--import qualified Data.HashMap.Strict  as HM (fromList)
import           File.Format.Fasta
import           File.Format.Fastc   hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus          (nexusStreamParser)
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
-- import File.Format.Conversion.ToInternal
import           PCG.Command.Types
import           PCG.Command.Types.Read.Internal
import           PCG.Command.Types.Read.Unification.Master
import           PCG.Script.Types
import           Prelude             hiding (lookup)
import           Text.Megaparsec

evaluate :: Command -> SearchState -> SearchState
{--}
evaluate (READ fileSpecs) old = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runEitherT . eitherTValidation $ parseSpecifiedFile <$> fileSpecs
    case result of
      Left err -> fail $ show err   -- Report structural errors here.
      Right xs -> mempty -- foldl (<>) old xs -- Here we validate more
{--}
evaluate _ _ = fail "Invalid READ command binding"
{--}
parseSpecifiedFile  :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseSpecifiedFile      (AnnotatedFile      _    ) = fail "Annotated file specification is not implemented"
parseSpecifiedFile      (ChromosomeFile     _    ) = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      (GenomeFile         _    ) = fail "Genome file specification is not implemented"
parseSpecifiedFile spec@(AminoAcidFile      _    ) = fastaAminoAcid spec
parseSpecifiedFile spec@(NucleotideFile     _    ) = fastaDNA       spec
parseSpecifiedFile spec@(CustomAlphabetFile{}    ) = parseCustomAlphabet spec
parseSpecifiedFile      (PrealignedFile     x tcmRef) = do
    tcm <- getSpecifiedTcm tcmRef
    res <- parseSpecifiedFile x
    case tcm of
      Nothing             -> pure res
      Just (path,content) -> do
        tcmMat <- hoistEither . first unparsable $ parse tcmStreamParser path content
        traverse (setTcm tcmMat) res 
  where
    setTcm :: TCM -> FracturedParseResult -> EitherT ReadError IO FracturedParseResult
    setTcm t fpr = case relatedTcm fpr of
                     Nothing -> pure $ fpr { relatedTcm = Just t }
                     Just _  -> fail "Multiple TCM files defined in prealigned file specification"
parseSpecifiedFile spec@(UnspecifiedFile    _    ) =
  getSpecifiedContent spec >>= eitherTValidation . fmap (progressiveParse . fst) . dataFiles

fastaDNA :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
fastaDNA spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent)
  where
    parseSpecifiedContent :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContent = eitherValidation . fmap parse' . dataFiles
    parse' :: FileResult -> Either ReadError FracturedParseResult
    parse' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first unparsable $ parse combinator path content
        combinator  = (\x -> fastaStreamConverter DNA x <|> fastaStreamConverter RNA x) =<< fastaStreamParser

-- TODO: abstract these two (three) v^
fastaAminoAcid :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
fastaAminoAcid spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent)
  where
    parseSpecifiedContent :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContent = eitherValidation . fmap parse' . dataFiles
    parse' :: FileResult -> Either ReadError FracturedParseResult
    parse' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first unparsable $ parse combinator path content
        combinator  = fastaStreamConverter AminoAcid =<< fastaStreamParser

parseCustomAlphabet :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseCustomAlphabet spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent)
  where
    parseSpecifiedContent :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContent specContent = do
        tcmMay <- case tcmFile specContent of
                    Nothing             -> pure Nothing
                    Just (path,content) -> bimap unparsable Just $ parse tcmStreamParser path content
        eitherValidation . fmap (parse' tcmMay) $ dataFiles specContent
    parse' m (path, content) = toFractured m path <$> parseResult
      where
        parseResult = first unparsable $ parse fastcStreamParser path content



--setTaxaSeqs :: HashMap Identifier ParsedSequences -> SearchState
--setTaxaSeqs x = pure $ Graph [mempty { parsedSeqs = x }]

--mapToHashMap :: (Eq k, Hashable k) => Map k v -> HashMap k v
--mapToHashMap = fromList . M.toList

--customToHashMap :: [FastcParseResult] -> HashMap Identifier CharacterSequence
--customToHashMap = fromList . concatMap (fmap (fastcLabel &&& fastcSymbols) . toList)
{-
parseSpecifiedFileSimple :: Parsec Text a -> (a -> FracturedParseResult-) -> FileSpecification -> EitherT ReadError IO SearchState
parseSpecifiedFileSimple comb toState spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent)
  where
    parseSpecifiedContent :: FileSpecificationContent -> Either ReadError SearchState
    parseSpecifiedContent = fmap toState . eitherValidation . fmap (first unparsable . parse') . dataFiles
    parse' (path,content) = parse comb path content
-}

progressiveParse :: FilePath -> EitherT ReadError IO FracturedParseResult
progressiveParse inputPath = do
    res0 <- liftIO . runEitherT . parseSpecifiedFile $ NucleotideFile [inputPath]
    case res0 of
      Right x -> pure $ head x
      Left  _ -> do
        res1 <- liftIO . runEitherT . parseSpecifiedFile $ AminoAcidFile [inputPath]
        case res1 of
          Right x -> pure $ head x
          Left  _ -> do
            (filePath, fileContent) <- head . dataFiles <$> getSpecifiedContent (UnspecifiedFile [inputPath])
            case parse newickStreamParser filePath fileContent of
              Right x -> pure $ toFractured Nothing filePath x
              Left  _ ->
                case parse verStreamParser filePath fileContent of
                  Right x -> pure $ toFractured Nothing filePath x
                  Left  _ ->
                    case parse tntStreamParser filePath fileContent of
                      Right x -> pure $ toFractured Nothing filePath x
                      Left  _ ->
                        case parse nexusStreamParser filePath fileContent of
                          Right x -> pure $ toFractured Nothing filePath x
                          Left  _ -> fail $ "Could not determine the file type of '" ++ filePath ++ "'. Try annotating the expected file data in the 'read' for more explicit error message on file parsing failures."

toFractured :: (Metadata a, ParsedCharacters a, ParseGraph a) => Maybe TCM -> FilePath -> a -> FracturedParseResult
toFractured tcmMat path = FPR <$> unifyCharacters  <*> unifyMetadata <*> unifyGraph <*> const tcmMat <*> const path
{--}
