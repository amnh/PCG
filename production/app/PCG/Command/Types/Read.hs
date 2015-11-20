module PCG.Command.Types.Read
  ( evaluate
  , validate
  ) where

import Control.Arrow       ((&&&))
import Control.Monad       (liftM2,when)
import Data.Char           (toLower)
import Data.Either         (partitionEithers)
import Data.Either.Custom  (isRight, rightMay)
import Data.Map            (toList)
import Data.Maybe          (fromJust,isNothing)
import Data.HashMap.Strict (fromList)
import Prelude      hiding (lookup)
import Text.Megaparsec
import System.Directory    (doesFileExist)

import File.Format.Fasta
import File.Format.Fastc
import File.Format.Newick
import File.Format.TransitionCostMatrix
import File.Format.VertexEdgeRoot

import PCG.Command.Types
import PCG.Command.Types.Read.Internal
import PCG.Evaluation
import PCG.Graph
import PCG.Script.Types

evaluate :: Command -> SearchState -> SearchState
evaluate (READ fileSpecs) old = do
    let paths = concatMap getSpecifiedFiles fileSpecs
    when (null paths) $ fail "The read command has files specified"
    exists <- evalIO . sequence $ fmap doesFileExist paths
    _      <- case fmap fst . filter (not . snd) $ zip paths exists of
                [x]    -> fail $ "The file  " ++ show  x     ++ " does not exist"
                (x:xs) -> fail $ "The files " ++ show (x:xs) ++ " do not exist"
                []     -> pure ()
    foldl smartParse old fileSpecs
evaluate _ _ = fail "Invalid READ command binding"

smartParse :: SearchState -> FileSpecification -> SearchState
smartParse ss (AminoAcidFile xs) = do
    contents <- evalIO . sequence $ parse' <$> xs
    parsed   <- case partitionEithers contents of
                  ([]    , ys) -> pure ys
                  ([e]   , _ ) -> fail $ "The following parse error was encountered: " ++ show e
                  (errors, _ ) -> fail . unlines . ("The following parse errors were encountered: ":) $ show <$> errors
    ss <> pure (mempty { taxaSeqs = convert parsed }) 
  where
    parse' filePath = parse (fastaStreamConverter AminoAcid =<< fastaStreamParser) filePath <$> readFile filePath
    convert         = fromList . concatMap toList
smartParse ss (NucleotideFile xs) = do
    contents <- evalIO . sequence $ parse' <$> xs
    parsed   <- case partitionEithers contents of
                  ([]    , ys) -> pure ys
                  ([e]   , _ ) -> fail $ "The following parse error was encountered: " ++ show e
                  (errors, _ ) -> fail . unlines . ("The following parse errors were encountered: ":) $ show <$> errors
    ss <> pure (mempty { taxaSeqs = convert parsed }) 
  where
    parse' filePath = parse (fastaStreamConverter DNA =<< fastaStreamParser) filePath <$> readFile filePath
    convert         = fromList . concatMap toList
smartParse _  (AnnotatedFile  _) = fail "Annotated file specification is not implemented"
smartParse _  (ChromosomeFile _) = fail "Chromosome file specification is not implemented"
smartParse _  (GenomeFile     _) = fail "Genome file specification is not implemented"
smartParse ss (CustomAlphabetFile files _ {-tcm-} _) = do
    contents <- evalIO . sequence $ parse' <$> files
    parsed   <- case partitionEithers contents of
                  ([]    , xs) -> pure xs
                  ([e]   , _ ) -> fail $ "The following parse error was encountered: " ++ show e
                  (errors, _ ) -> fail . unlines . ("The following parse errors were encountered: ":) $ show <$> errors
{-
    _        <- case tcm of
                 Just tcm' -> evalIO $ parse tcmStreamParser tcm' <$> readFile tcm' -- ignore the matrix for now
                 Nothing   -> pure $ Right mempty
-}
    ss <> pure (mempty { taxaSeqs = convert parsed }) 
  where
    parse' filePath = parse fastcStreamParser filePath <$> readFile filePath
    convert         = fromList . concatMap (fmap (fastcLabel &&& fastcSymbols))
smartParse ss (PrealignedFile spec _ {-tcm-}) = do
    res <- smartParse ss spec
{-
    _   <- case tcm of
             Just tcm' -> evalIO $ parse tcmStreamParser tcm' <$> readFile tcm' -- ignore the matrix for now
             Nothing   -> pure $ Right mempty
-}
    pure res
smartParse ss (UnspecifiedFile xs) = do
    foldl (<>) ss (progressiveParse <$> xs)
--  where
--    parse' filePath = parse (fastaStreamConverter DNA =<< fastaStreamParser) filePath <$> readFile filePath
--    convert         = fromList . concatMap toList

progressiveParse :: FilePath -> SearchState
progressiveParse path = do
    content <- evalIO $ readFile path
    case partitionEithers $ parseTryOrderForSequences <*> [content] of
      (_ , parsed:_) -> pure (mempty { taxaSeqs = fromList $ toList parsed })
      (_ , []      ) ->
        case parse newickStreamParser path content of
          Right _ -> pure mempty
          Left  _ ->
            case parse tcmStreamParser path content of
              Right _ -> pure mempty
              Left  _ ->
                case parse verStreamParser path content of
                  Right _ -> pure mempty
                  Left  _ -> fail $ "Could not determine the file type of '"++path++"'. Try annotating the expected file data in the 'read' for more explicit error message on file prsing failures."
  where
    parseTryOrderForSequences :: [FilePath -> Either ParseError TaxonSequenceMap]
    parseTryOrderForSequences =
      [ parse (fastaStreamConverter DNA       =<< fastaStreamParser) path
      , parse (fastaStreamConverter RNA       =<< fastaStreamParser) path
      , parse (fastaStreamConverter AminoAcid =<< fastaStreamParser) path
      ]

validate :: [Argument] -> Either String Command
validate xs =
  case partitionEithers $ validateReadArg <$> xs of
    ([]  , []) -> Left  $ "No arguments provided to the 'read' command! The 'read' command expects one or more arguments"
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
    files'  = (fromJust . rightMay . primativeString) <$> files
    options = getCustomAlphabetOption <$> tail suffix
    badOption = any isNothing options
    tcmFile = case suffix of
               (LidentNamedArg (Lident y) ys):_ -> if "tcm" == (toLower <$> y)
                                                   then either (const Nothing) Just $ primativeString ys
                                                   else Nothing
               _                                -> Nothing

    subDefinition
      |   null xs
      || (null.tail) xs     = Left  "Missing minimum arguments of at least one file path containing data and file path to tcm definition"
      |   null suffix
      ||  isNothing tcmFile = Left  "Missing filepath to tcm definition"
      |   badOption         = Left  "One or more optional arguments are invalid"
      | otherwise           = case partitionOptions $ fromJust <$> options of
                                ([] ,[] , []) -> Right $ CustomAlphabetFile files' tcmFile []
                                ([a],[] , []) -> Right $ CustomAlphabetFile files' tcmFile [a]
                                ([] ,[b], []) -> Right $ CustomAlphabetFile files' tcmFile [b]
                                ([] ,[] ,[c]) -> Right $ CustomAlphabetFile files' tcmFile [c]
                                ([a],[b], []) -> Right $ CustomAlphabetFile files' tcmFile [a,b]
                                ([a],[] ,[c]) -> Right $ CustomAlphabetFile files' tcmFile [a,c]
                                ([] ,[b],[c]) -> Right $ CustomAlphabetFile files' tcmFile [b,c]
                                ([a],[b],[c]) -> Right $ CustomAlphabetFile files' tcmFile [a,b,c]
                                _             -> Left "Multiple labeled arguments sharing the same label"

validateReadArg (LidentNamedArg (Lident identifier) (ArgumentList (arg:args))) | "prealigned" == (toLower <$> identifier) =
  case args of
    []                               -> flip PrealignedFile Nothing <$> val
    [(LidentNamedArg (Lident x) xs)] -> case toLower <$> x of
                                          "tcm" -> liftM2 PrealignedFile val (Just <$> primativeString xs)
                                          _     -> Left  $ "Unexpected named argument '" ++ x ++ "'"
    _                                -> Left  "Too many arguments"
  where
    val = validateReadArg arg

validateReadArg _ = Left "Unknown argument in read command"

partitionOptions :: [CustomAlphabetOptions] -> ([CustomAlphabetOptions],[CustomAlphabetOptions],[CustomAlphabetOptions])
partitionOptions xs = foldr f ([],[],[]) xs
  where
    f e@(Init3D     _) (x,y,z) = (e:x,  y,  z)
    f e@(Level    _ _) (x,y,z) = (  x,e:y,  z)
    f e@(Tiebreaker _) (x,y,z) = (  x,  y,e:z)
        
getCustomAlphabetOption :: Argument -> Maybe CustomAlphabetOptions
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (PrimativeArg (BitValue b)))
  | "init3d"     == (toLower <$> identifier) = Just $ Init3D b
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (ArgumentList [(PrimativeArg (WholeNum n)),(PrimativeArg (TextValue x))]))
  |  "level"     == (toLower <$> identifier) = Level n <$> getCustomAlphabetStrategy x
getCustomAlphabetOption (LidentNamedArg (Lident identifier) (PrimativeArg (TextValue x)))
  | "tiebreaker" == (toLower <$> identifier) = Tiebreaker <$> getCustomAlphabetStrategy x
getCustomAlphabetOption _ = Nothing

getCustomAlphabetStrategy :: [Char] -> Maybe CustomAlphabetStrategy
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

primativeStringErrorSuffix :: [Char]
primativeStringErrorSuffix = "found where a string argument containing a file path was expected"

--smartParse (filePath, fileData) = parse (fastaStreamConverter DNA =<< fastaStreamParser) filePath fileData
--smartParse (,) = 
