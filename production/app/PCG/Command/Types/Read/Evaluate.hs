{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph
import           Bio.Sequence.Parsed
import           Control.Arrow              ((&&&))
import           Control.Monad              (liftM2,when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Evaluation
import           Data.Bifunctor             (first)
import           Data.Char                  (toLower)
import           Data.Either                (partitionEithers)
import           Data.Either.Combinators    (isRight, rightToMaybe)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Map                   (Map)
import           Data.Maybe                 (fromJust,isNothing)
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict  as HM (fromList)
import           Data.Text.Lazy             (Text)
import           File.Format.Fasta
import           File.Format.Fastc   hiding (Identifier)
import           File.Format.Newick
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
{-
evaluate (READ fileSpecs) old = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runEitherT . eitherTValidation $ parseSpecifiedFile <$> fileSpecs
    case result of
      Left err -> fail $ show err   -- Report structural errors here.
      Right xs -> foldl (<>) old xs -- Here we validate more
-}
evaluate _ _ = fail "Invalid READ command binding"
{-
parseSpecifiedFile  :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseSpecifiedFile      (AnnotatedFile      _    ) = fail "Annotated file specification is not implemented"
parseSpecifiedFile      (ChromosomeFile     _    ) = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      (GenomeFile         _    ) = fail "Genome file specification is not implemented"
-- Currently ignoring TCM
parseSpecifiedFile      (PrealignedFile     x _  ) = parseSpecifiedFile x

-- Currently ignoring TCM
parseSpecifiedFile spec@(CustomAlphabetFile{}    ) = 
  parseSpecifiedFileSimple  fastcStreamParser (setTaxaSeqs . customToHashMap) spec
parseSpecifiedFile spec@(AminoAcidFile      _    ) =
  parseSpecifiedFileSimple (fastaStreamConverter AminoAcid =<< fastaStreamParser) (setTaxaSeqs . fmap mapToHashMap) spec
parseSpecifiedFile spec@(NucleotideFile     _    ) =
  parseSpecifiedFileSimple (fastaStreamConverter DNA       =<< fastaStreamParser) (setTaxaSeqs . fmap mapToHashMap) spec
parseSpecifiedFile spec@(UnspecifiedFile    _    ) =
  getSpecifiedContent spec >>= fmap (foldl (<>) mempty) . eitherTValidation . fmap progressiveParse . dataFiles

setTaxaSeqs :: HashMap Identifier ParsedSequences -> SearchState
setTaxaSeqs x = pure $ Graph [mempty { parsedSeqs = x }]

mapToHashMap :: (Eq k, Hashable k) => Map k v -> HashMap k v
mapToHashMap = fromList . M.toList

customToHashMap :: [FastcParseResult] -> HashMap Identifier CharacterSequence
customToHashMap = fromList . concatMap (fmap (fastcLabel &&& fastcSymbols) . toList)

parseSpecifiedFileSimple :: Parsec Text a -> (a -> FracturedParseResult-) -> FileSpecification -> EitherT ReadError IO SearchState
parseSpecifiedFileSimple comb toState spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent)
  where
    parseSpecifiedContent :: FileSpecificationContent -> Either ReadError SearchState
    parseSpecifiedContent = fmap toState . eitherValidation . fmap (first unparsable . parse') . dataFiles
    parse' (path,content) = parse comb path content

progressiveParse :: FileResult -> EitherT ReadError IO SearchState
progressiveParse (filePath, fileContent) =
  case snd . partitionEithers $ parseTryOrderForSequences <*> [fileContent] of
    parsed:_ -> pure . pure $ Graph [mempty { parsedSeqs = mapsToHashMap [parsed] }]
    []       ->
--      case parse nexusStreamParser filePath fileContent of
--        Right _ -> pure mempty
--        Left  _ ->
          case parse newickStreamParser filePath fileContent of
            Right _ -> pure mempty
            Left  _ ->
              case parse tcmStreamParser  filePath fileContent of
                Right _ -> pure mempty
                Left  _ ->
                  case parse verStreamParser filePath fileContent of
                    Right _ -> pure mempty
                    Left  _ -> fail $ "Could not determine the file type of '" ++ filePath ++ "'. Try annotating the expected file data in the 'read' for more explicit error message on file parsing failures."
  where
    parseTryOrderForSequences :: [FileContent -> Either ParseError TaxonSequenceMap]
    parseTryOrderForSequences =
      [ parse (fastaStreamConverter DNA       =<< fastaStreamParser) filePath
      , parse (fastaStreamConverter RNA       =<< fastaStreamParser) filePath
      , parse (fastaStreamConverter AminoAcid =<< fastaStreamParser) filePath
      ]
-}
