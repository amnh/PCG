module PCG.Command.Types.Read
  ( evaluate
  , validate
  ) where

import Control.Monad       (when)
import Data.Either         (partitionEithers)
import Data.Map            (toList)
import Data.HashMap.Strict (fromList)
import Prelude      hiding (lookup)
import Text.Megaparsec
import System.Directory    (doesFileExist)

import File.Format.Fasta

import PCG.Command.Internal
import PCG.Command.Types
import PCG.Evaluation
import PCG.Graph
import PCG.Script.Types

evaluate :: Command -> SearchState -> SearchState
evaluate (READ paths) old = old >>= f
  where
    f g = do
        when (null paths) $ fail "The read command has files" 
        exists   <- evalIO . sequence $ doesFileExist <$> paths
        contents <- case fmap fst . filter (not . snd) $ zip paths exists of
                      [x]    -> fail $ "The file  " ++ show  x     ++ " does not exist"
                      (x:xs) -> fail $ "The files " ++ show (x:xs) ++ " do not exist"
                      []     -> evalIO . sequence $ readFile <$> paths
        parsed   <- case partitionEithers $ parse' <$> (zip paths contents) of
                      ([]    , xs) -> pure xs
                      ([e]   , _ ) -> fail $ "The following parse error was encountered: " ++ show e
                      (errors, _ ) -> fail . unlines . ("The following parse errors were encountered: ":) $ show <$> errors
        pure $ g <> (mempty { taxaSeqs = convert parsed })
      where
        parse' :: (String, String) -> Either ParseError TaxonSequenceMap 
        parse' (filePath,fileData) = parse (fastaStreamConverter DNA =<< fastaStreamParser) filePath fileData
        convert = fromList . concatMap toList


evaluate _ _ = fail "Invalid READ command binding"

validate :: [Argument] -> Either String Command
validate xs
  | noArgs        = Left "No arguments provided to the 'read' command! The 'read' command expects one or more arguments"
  | notAllStrings = Left "One or more arguments provided to the 'read' command are not strings! The 'read' command expects one or more string arguments."
  | otherwise     = Right $ READ s
  where
    noArgs        = null xs
    x@(p,_,_,_,_) = partitionArguments xs
    y@(_,_,_,s,_) = partitionPrimatives p
    notAllStrings = case (x,y) of
                      ((_,[],[],[],[]),([],[],[],_,[])) -> False
                      _                                 -> True

