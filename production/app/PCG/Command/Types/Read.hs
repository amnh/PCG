{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Read
  ( evaluate
  , validate
  ) where

import Bio.Phylogeny.Graph
import Control.Arrow              ((&&&))
import Control.Monad              (liftM2,when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Evaluation
import Data.Bifunctor             (first)
import Data.Char                  (toLower)
import Data.Either                (partitionEithers)
import Data.Either.Combinators    (isRight, rightToMaybe)
import Data.Either.Custom
import Data.Foldable              (toList)
import Data.Map                   (Map)
import qualified Data.Map as M    (toList)
import Data.Maybe                 (fromJust,isNothing)
import Data.Hashable
import Data.HashMap.Strict        (HashMap,fromList)
import Data.Text.Lazy             (Text)
import Prelude             hiding (lookup)
import Text.Megaparsec

import File.Format.Fasta
import File.Format.Fastc   hiding (Identifier)
import File.Format.Newick
import File.Format.TransitionCostMatrix
import File.Format.VertexEdgeRoot
-- import File.Format.Conversion.ToInternal

import PCG.Command.Types
import PCG.Command.Types.Read.Internal
import PCG.Script.Types


evaluate :: Command -> SearchState -> SearchState
evaluate (READ fileSpecs) old = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    result <- liftIO . runEitherT . eitherTValidation $ parseSpecifiedFile <$> fileSpecs
    case result of
      Left err -> fail $ show err
      Right xs -> foldl (<>) old xs
evaluate _ _ = fail "Invalid READ command binding"

parseSpecifiedFile  :: FileSpecification -> EitherT ReadError IO SearchState
-- Currently ignoring TCM
parseSpecifiedFile      (PrealignedFile     x _  ) = parseSpecifiedFile x
-- Currently ignoring TCM
parseSpecifiedFile spec@(CustomAlphabetFile{}    ) = 
  parseSpecifiedFileSimple  fastcStreamParser (setTaxaSeqs . customToHashMap) spec
parseSpecifiedFile spec@(AminoAcidFile      _    ) =
  parseSpecifiedFileSimple (fastaStreamConverter AminoAcid =<< fastaStreamParser) (setTaxaSeqs . mapsToHashMap) spec
parseSpecifiedFile spec@(NucleotideFile     _    ) =
  parseSpecifiedFileSimple (fastaStreamConverter DNA       =<< fastaStreamParser) (setTaxaSeqs . mapsToHashMap) spec
parseSpecifiedFile spec@(UnspecifiedFile    _    ) =
  getSpecifiedContent spec >>= fmap (foldl (<>) mempty) . eitherTValidation . fmap progressiveParse . dataFiles
parseSpecifiedFile      (AnnotatedFile      _    ) = fail "Annotated file specification is not implemented"
parseSpecifiedFile      (ChromosomeFile     _    ) = fail "Chromosome file specification is not implemented"
parseSpecifiedFile      (GenomeFile         _    ) = fail "Genome file specification is not implemented"

setTaxaSeqs :: HashMap Identifier Sequence -> SearchState
setTaxaSeqs x = pure $ Graph [mempty { parsedSeqs = x }]

mapsToHashMap :: (Eq k, Hashable k) => [Map k v] -> HashMap k v
mapsToHashMap = fromList . concatMap M.toList

customToHashMap :: [FastcParseResult] -> HashMap Identifier CharacterSequence
customToHashMap = fromList . concatMap (fmap (fastcLabel &&& fastcSymbols) . toList)

parseSpecifiedFileSimple :: Parsec Text a -> ([a] -> SearchState) -> FileSpecification -> EitherT ReadError IO SearchState
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

validate :: [Argument] -> Either String Command
validate xs =
  case partitionEithers $ validateReadArg <$> xs of
    ([]  , []) -> Left "No arguments provided to the 'read' command! The 'read' command expects one or more arguments"
    (y:ys, _ ) -> Left  $ unlines (y:ys)
    ([]  , ys) -> Right $ READ ys

validateReadArg :: Argument -> Either String FileSpecification
validateReadArg (PrimativeArg   (TextValue str))   = Right $ UnspecifiedFile [str]
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | (\x -> x == "aminoacid"  || x == "aminoacids"     ) $ toLower <$> identifier =
  case partitionEithers $ primativeString <$> xs of
    ([]    , filePaths) -> Right $ AminoAcidFile  filePaths
    (errors, _        ) -> Left  $ unlines errors
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | (\x -> x == "nucleotide" || x == "nucleotides"    ) $ toLower <$> identifier =
  case partitionEithers $ primativeString <$> xs of
    ([]    , filePaths) -> Right $ NucleotideFile filePaths
    (errors, _        ) -> Left  $ unlines errors
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | "annotated"  == (toLower <$> identifier) =
  case partitionEithers $ primativeString <$> xs of
    ([]    , filePaths) -> Right $ AnnotatedFile  filePaths
    (errors, _        ) -> Left  $ unlines errors
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | (\x -> x == "chomosome"  || x == "chromosomes"    ) $ toLower <$> identifier =
  case partitionEithers $ primativeString <$> xs of
    ([]    , filePaths) -> Right $ ChromosomeFile filePaths
    (errors, _        ) -> Left  $ unlines errors
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | (\x -> x == "genome"     || x == "genomes"        ) $ toLower <$> identifier =
  case partitionEithers $ primativeString <$> xs of
    ([]    , filePaths) -> Right $ GenomeFile     filePaths
    (errors, _        ) -> Left  $ unlines errors
validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList xs)) | (\x -> x == "breakinv"   || x == "custom_alphabet") $ toLower <$> identifier = subDefinition
  where
    (files,suffix) = span (isRight . primativeString) xs
    files'  = (fromJust . rightToMaybe . primativeString) <$> files
    options = getCustomAlphabetOption <$> tail suffix
    badOption = any isNothing options
    tcmFile' = case suffix of
                LidentNamedArg (Lident y) ys :_ -> if "tcm" == (toLower <$> y)
                                                   then either (const Nothing) Just $ primativeString ys
                                                   else Nothing
                _                               -> Nothing

    subDefinition
      |   null xs
      || (null.tail) xs      = Left  "Missing minimum arguments of at least one file path containing data and file path to tcm definition"
      |   null suffix
      ||  isNothing tcmFile' = Left  "Missing filepath to tcm definition"
      |   badOption          = Left  "One or more optional arguments are invalid"
      | otherwise            = case partitionOptions $ fromJust <$> options of
                                 ([] ,[] , []) -> Right $ CustomAlphabetFile files' tcmFile' []
                                 ([a],[] , []) -> Right $ CustomAlphabetFile files' tcmFile' [a]
                                 ([] ,[b], []) -> Right $ CustomAlphabetFile files' tcmFile' [b]
                                 ([] ,[] ,[c]) -> Right $ CustomAlphabetFile files' tcmFile' [c]
                                 ([a],[b], []) -> Right $ CustomAlphabetFile files' tcmFile' [a,b]
                                 ([a],[] ,[c]) -> Right $ CustomAlphabetFile files' tcmFile' [a,c]
                                 ([] ,[b],[c]) -> Right $ CustomAlphabetFile files' tcmFile' [b,c]
                                 ([a],[b],[c]) -> Right $ CustomAlphabetFile files' tcmFile' [a,b,c]
                                 _             -> Left "Multiple labeled arguments sharing the same label"

validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList (arg:args))) | "prealigned" == (toLower <$> identifier) =
  case args of
    []                             -> flip PrealignedFile Nothing <$> val
    [LidentNamedArg (Lident x) xs] -> case toLower <$> x of
                                        "tcm" -> liftM2 PrealignedFile val (Just <$> primativeString xs)
                                        _     -> Left $ "Unexpected named argument '" ++ x ++ "'"
    _                              -> Left "Too many arguments"
  where
    val = validateReadArg arg
validateReadArg _ = Left "Unknown argument in read command"

partitionOptions :: [CustomAlphabetOptions] -> ([CustomAlphabetOptions],[CustomAlphabetOptions],[CustomAlphabetOptions])
partitionOptions = foldr f ([],[],[])
  where
    f e@(Init3D     _) (x,y,z) = (e:x,  y,  z)
    f e@(Level    _ _) (x,y,z) = (  x,e:y,  z)
    f e@(Tiebreaker _) (x,y,z) = (  x,  y,e:z)
        
getCustomAlphabetOption :: Argument -> Maybe CustomAlphabetOptions
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (PrimativeArg (BitValue b)))
  | "init3d"     == (toLower <$> identifier) = Just $ Init3D b
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (ArgumentList [PrimativeArg (WholeNum n), PrimativeArg (TextValue x)]))
  |  "level"     == (toLower <$> identifier) = Level n <$> getCustomAlphabetStrategy x
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (PrimativeArg (TextValue x)))
  | "tiebreaker" == (toLower <$> identifier) = Tiebreaker <$> getCustomAlphabetStrategy x
getCustomAlphabetOption _ = Nothing

getCustomAlphabetStrategy :: String -> Maybe CustomAlphabetStrategy
getCustomAlphabetStrategy x
  | x' == "first"     = Just First
  | x' == "last"      = Just Last
  | x' == "at_random" = Just AtRandom
  | otherwise         = Nothing
  where
    x' = toLower <$> x

primativeString :: Argument -> Either String FilePath
primativeString (PrimativeArg   (TextValue str)) = Right str
primativeString (PrimativeArg   _              ) = Left $ "A primative value that is not a file path " ++ primativeStringErrorSuffix
primativeString (LidentArg      (Lident i)     ) = Left $ "Identifier '"       ++ i ++ "' " ++ primativeStringErrorSuffix
primativeString (LidentNamedArg (Lident i) _   ) = Left $ "Labeled argument '" ++ i ++ "' " ++ primativeStringErrorSuffix
primativeString (CommandArg     _              ) = Left $ "Command argument "  ++              primativeStringErrorSuffix
primativeString (ArgumentList   _              ) = Left $ "Argument list "     ++              primativeStringErrorSuffix

primativeStringErrorSuffix :: String
primativeStringErrorSuffix = "found where a string argument containing a file path was expected"

