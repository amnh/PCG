{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Read.Evaluate
  ( evaluate
  ) where

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.Graph.Parsed
import           Bio.Phylogeny.PhyloCharacter
import           Bio.Metadata.Class
--import           Bio.Metadata.MaskGenerator
import           Bio.Sequence.Parsed
import           Bio.Sequence.Parsed.Class
import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Evaluation
import           Data.Bifunctor             (bimap,first)
import           Data.Char                  (isLower,toLower,isUpper,toUpper)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Key                   ((!),lookup)
import           Data.Map                   (Map,assocs,insert,union)
import qualified Data.Map              as M (fromList)
import           Data.Maybe                 (fromMaybe)
--import           Data.Hashable
--import           Data.HashMap.Strict        (HashMap)
--import qualified Data.HashMap.Strict  as HM (fromList)
import           Data.Vector                (Vector)
import qualified Data.Vector           as V (zipWith)
import           Debug.Trace
import           File.Format.Fasta   hiding   (FastaSequenceType(..))
import qualified File.Format.Fasta   as Fasta (FastaSequenceType(..))
import           File.Format.Fastc   hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus          (nexusStreamParser)
import           File.Format.TNT     hiding (casei)
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
-- import File.Format.Conversion.ToInternal
import           PCG.Command.Types (Command(..))
import           PCG.Command.Types.Read.Internal
import           PCG.Command.Types.Read.Unification.Master
--import           PCG.Script.Types
import           Prelude             hiding (lookup)
import           Text.Megaparsec

evaluate :: Command -> SearchState -> SearchState
{--}
--evaluate (READ fileSpecs) _old | trace ("Evaluated called: " <> show fileSpecs) False = undefined
evaluate (READ fileSpecs) _old = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runEitherT . eitherTValidation $ parseSpecifiedFile <$> fileSpecs
    case result of
      Left pErr -> fail $ show pErr   -- Report structural errors here.
      Right xs ->
        case masterUnify' $ transformation <$> concat xs of
          Left uErr -> fail $ show uErr -- Report rectification errors here.
          Right g   -> pure g           -- TODO: rectify against 'old' SearchState, don't just blindly merge or ignore old state
  where
    transformation = expandIUPAC . prependFilenamesToCharacterNames . applyReferencedTCM

evaluate _ _ = fail "Invalid READ command binding"
{--}
parseSpecifiedFile  :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseSpecifiedFile      AnnotatedFile     {}     = fail "Annotated file specification is not implemented"
parseSpecifiedFile      ChromosomeFile    {}     = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      GenomeFile        {}     = fail "Genome file specification is not implemented"
parseSpecifiedFile spec@AminoAcidFile     {}     = fastaAminoAcid spec
parseSpecifiedFile spec@NucleotideFile    {}     = fastaDNA       spec
parseSpecifiedFile spec@CustomAlphabetFile{}     = parseCustomAlphabet spec
parseSpecifiedFile     (PrealignedFile x tcmRef) = do
    tcmContent <- getSpecifiedTcm tcmRef
    subContent <- parseSpecifiedFile x
    fmap (fmap setCharactersToAligned) $
      case tcmContent of
        Nothing             -> pure subContent
        Just (path,content) -> do
          tcmMat <- hoistEither . first unparsable $ parse tcmStreamParser path content
          traverse (setTcm tcmMat) subContent
  where
    setTcm :: TCM -> FracturedParseResult -> EitherT ReadError IO FracturedParseResult
    setTcm t fpr = case relatedTcm fpr of
                     Nothing -> pure $ fpr { relatedTcm = Just t }
                     Just _  -> fail "Multiple TCM files defined in prealigned file specification"
parseSpecifiedFile spec@(UnspecifiedFile    _    ) =
  getSpecifiedContent spec >>= eitherTValidation . fmap (progressiveParse . fst) . dataFiles

fastaDNA :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
--fastaDNA spec | trace ("fasta DNA parser with spec " ++ show spec) False = undefined
fastaDNA spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent parse')
  where
    parse' :: FileResult -> Either ReadError FracturedParseResult
    parse' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = {- (\x -> trace (show x) x) . -} first unparsable $ parse combinator path content
        combinator  = (\x -> try (fastaStreamConverter Fasta.DNA x) <|> fastaStreamConverter Fasta.RNA x) =<< fastaStreamParser

-- TODO: abstract these two (three) v^
fastaAminoAcid :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
fastaAminoAcid spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContent parse')
  where
    parse' :: FileResult -> Either ReadError FracturedParseResult
    parse' (path,content) = toFractured Nothing path <$> parseResult
      where
        parseResult = first unparsable $ parse combinator path content
        combinator  = fastaStreamConverter Fasta.AminoAcid =<< fastaStreamParser

parseSpecifiedContent :: (FileResult -> Either ReadError FracturedParseResult) -> FileSpecificationContent -> Either ReadError [FracturedParseResult]
parseSpecifiedContent parse' = eitherValidation . fmap parse' . dataFiles

parseCustomAlphabet :: FileSpecification -> EitherT ReadError IO [FracturedParseResult]
parseCustomAlphabet spec = getSpecifiedContent spec >>= (hoistEither . parseSpecifiedContentWithTcm)
  where
    parseSpecifiedContentWithTcm :: FileSpecificationContent -> Either ReadError [FracturedParseResult]
    parseSpecifiedContentWithTcm specContent = do
        tcmMay <- case tcmFile specContent of
                    Nothing             -> pure Nothing
                    Just (path,content) -> bimap unparsable Just $ parse tcmStreamParser path content
        eitherValidation . fmap (parse' tcmMay) $ dataFiles specContent
    parse' m (path, content) = toFractured m path <$> parseResult
      where
        parseResult = first unparsable $ parse fastcStreamParser path content

applyReferencedTCM :: FracturedParseResult -> FracturedParseResult
applyReferencedTCM fpr =
  case relatedTcm fpr of
     Nothing -> fpr
     Just x  -> let newAlphabet = toList $ customAlphabet x
                    newTcm      = transitionCosts x
                in  fpr { parsedMetas = updateAlphabet newAlphabet . updateTcm newTcm <$> parsedMetas fpr }

prependFilenamesToCharacterNames :: FracturedParseResult -> FracturedParseResult
prependFilenamesToCharacterNames fpr = fpr { parsedMetas = prependName (sourceFile fpr) <$> parsedMetas fpr }

setCharactersToAligned :: FracturedParseResult -> FracturedParseResult
setCharactersToAligned fpr = fpr { parsedMetas = updateAligned True <$> parsedMetas fpr }

expandIUPAC :: FracturedParseResult -> FracturedParseResult
expandIUPAC fpr = fpr { parsedChars = newTreeSeqs }
  where
    newTreeSeqs = f (parsedChars fpr) (parsedMetas fpr)
    f :: TreeSeqs -> Vector CharInfo -> TreeSeqs
    f mapping meta = g <$> mapping
      where
        g :: ParsedSequences -> ParsedSequences
        g = V.zipWith h meta 
          where
            h :: CharInfo -> Maybe ParsedSeq -> Maybe ParsedSeq
            h cInfo seqMay = expandCodes <$> seqMay
              where
                expandCodes :: ParsedSeq -> ParsedSeq
                expandCodes x =
                  case cInfo of
                    DNA{}       -> expandOrId nucleotideIUPAC <$> x
                    RNA{}       -> expandOrId nucleotideIUPAC <$> x
                    AminoAcid{} -> expandOrId aminoAcidIUPAC  <$> x
                    _           -> x
    expandOrId m x = fromMaybe x $ x `lookup` m

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
    nukeResult <- liftIO . runEitherT . parseSpecifiedFile $ NucleotideFile [inputPath]
    case nukeResult of
      Right x -> pure $ head x
      Left  _ -> do
        aminoAcidResult <- liftIO . runEitherT . parseSpecifiedFile $ AminoAcidFile [inputPath]
        case aminoAcidResult of
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
toFractured tcmMat path = FPR <$> unifyCharacters
                              <*> unifyMetadata
                              <*> unifyGraph
                              <*> const tcmMat
                              <*> const path
{--}

nucleotideIUPAC :: Map AmbiguityGroup AmbiguityGroup
nucleotideIUPAC = casei core
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

aminoAcidIUPAC :: Map AmbiguityGroup AmbiguityGroup
aminoAcidIUPAC = casei $ core `union` multi
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

casei :: Map AmbiguityGroup v -> Map AmbiguityGroup v
casei x = foldl f x $ assocs x
  where
    f m ([[k]], v)
      | isLower k  = insert [[toUpper k]] v m
      | isUpper k  = insert [[toLower k]] v m
      | otherwise  = m
    f m (_    , _) = m

                                                                    
