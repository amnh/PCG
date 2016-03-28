{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.Validate
  ( validate
  ) where

import Control.Monad              (liftM2)
import Data.Char                  (toLower)
import Data.Either                (partitionEithers)
import Data.Either.Combinators    (isRight, rightToMaybe)
import Data.Maybe                 (fromJust,isNothing)
import PCG.Command.Types
import PCG.Command.Types.Report.Internal
import PCG.Script.Types           (Argument(..),Lident(..),Primative(..))

validate :: [Argument] -> Either String Command
validate xs =
  case partitionEithers $ validateReportArg <$> xs of
    (  [], []) -> Left "No arguments provided to the 'report' command! The 'report' command expects one or more arguments"
    (y:ys,  _) -> Left  $ unlines (y:ys)
    (  [], ys) ->
      case partitionEithers ys of
        (    [], [format]) -> Right $ REPORT  OutputToStdout     format
        ([path], [format]) -> Right $ REPORT (OutputToFile path) format
        _                  -> Left $ "SO BAD, BETTER ERROR LATER. In Report."

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

primativeString :: Argument -> Either String FilePath
primativeString (PrimativeArg   (TextValue str)) = Right str
primativeString (PrimativeArg   _              ) = Left $ "A primative value that is not a file path " ++ primativeStringErrorSuffix
primativeString (LidentArg      (Lident i)     ) = Left $ "Identifier '"       ++ i ++ "' " ++ primativeStringErrorSuffix
primativeString (LidentNamedArg (Lident i) _   ) = Left $ "Labeled argument '" ++ i ++ "' " ++ primativeStringErrorSuffix
primativeString (CommandArg     _              ) = Left $ "Command argument "  ++              primativeStringErrorSuffix
primativeString (ArgumentList   _              ) = Left $ "Argument list "     ++              primativeStringErrorSuffix

-- TODO: Make this have many different descriptive messages
getSingltonArgumentList :: Argument -> Either String Argument
getSingltonArgumentList (ArgumentList   [x]     ) = Right x
getSingltonArgumentList _  = Left "Not a singlton argument list"

primativeStringErrorSuffix :: String
primativeStringErrorSuffix = "found where a string argument containing a file path was expected"

