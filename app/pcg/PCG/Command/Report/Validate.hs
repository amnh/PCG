{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.Validate
  ( validate
  ) where

import Data.Char                         (toLower)
import Data.Either                       (partitionEithers)
import Data.Monoid                       ((<>))
import PCG.Command.Types
import PCG.Command.Types.Report.Internal
import PCG.Script.Types


validate :: [Argument] -> Either String Command
validate xs =
  case partitionEithers $ validateReportArg <$> xs of
    (  [], []) -> Left "No arguments provided to the 'report' command! The 'report' command expects one or more arguments"
    (y:ys,  _) -> Left  $ unlines (y:ys)
    (  [], ys) ->
      case partitionEithers ys of
        (    [],       []) -> Right $ REPORT  OutputToStdout     Data
        (    [], [format]) -> Right $ REPORT  OutputToStdout     format
        ([path],       []) -> Right $ REPORT (OutputToFile path) Data
        ([path], [format]) -> Right $ REPORT (OutputToFile path) format
        (    ps,       fs) -> let psErr = if moreThanSingleton ps then "Found multiple file paths for output: "    <> show ps else []
                                  fsErr = if moreThanSingleton fs then "Found multiple output formats specified: " <> show fs else []
                              in Left $ unlines [psErr,fsErr]
  where
    moreThanSingleton (_:_:_) = True
    moreThanSingleton _       = False


validateReportArg :: Argument -> Either String (Either FileName OutputFormat)
validateReportArg (PrimativeArg   (TextValue str))   = Right $ Left str
validateReportArg (LidentArg (Lident identifier))
  | (=="cross_references") $ toLower <$> identifier = Right . Right $ CrossReferences []
validateReportArg (LidentNamedArg (Lident identifier) (LidentNamedArg (Lident tok) (ArgumentList xs)))
  |  "cross_references" == (toLower <$> identifier)
  && "names"            == (toLower <$> tok) =
    case partitionEithers $ primativeString <$> xs of
      ([]    , fileNames) -> Right . Right $ CrossReferences fileNames
      (errors, _        ) -> Left $ unlines errors
validateReportArg (LidentArg (Lident identifier))
  |  "data" == (toLower <$> identifier) = Right $ Right Data
validateReportArg (LidentArg (Lident identifier))
  |  "dot"      == (toLower <$> identifier)
  || "graphviz" == (toLower <$> identifier) = Right $ Right DotFile
validateReportArg (LidentArg (Lident identifier))
  |  "metadata" == (toLower <$> identifier) = Right $ Right Metadata
validateReportArg (LidentArg (Lident identifier))
  |  "dynamic_table" == (toLower <$> identifier) = Right $ Right DynamicTable
validateReportArg (LidentArg (Lident identifier))
  |  "implied_alignment" == (toLower <$> identifier) = Right $ Right ImpliedAlignmentCharacters

validateReportArg x = Left $ "Unrecognized report command(s): " <> show x


primativeString :: Argument -> Either String FilePath
primativeString (PrimativeArg   (TextValue str)) = Right str
primativeString (PrimativeArg   _              ) = Left $ "A primitive value that is not a file path " <> primativeStringErrorSuffix
primativeString (LidentArg      (Lident i)     ) = Left $ "Identifier '"       <> i <> "' " <> primativeStringErrorSuffix
primativeString (LidentNamedArg (Lident i) _   ) = Left $ "Labeled argument '" <> i <> "' " <> primativeStringErrorSuffix
primativeString (CommandArg     _              ) = Left $ "Command argument "  <>              primativeStringErrorSuffix
primativeString (ArgumentList   _              ) = Left $ "Argument list "     <>              primativeStringErrorSuffix


primativeStringErrorSuffix :: String
primativeStringErrorSuffix = "found where a string argument containing a file path was expected"
