
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Main (main) where


import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Control.Lens
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                       as BM
import qualified Data.List.NonEmpty               as NE
import           System.Environment               (getArgs)
import           Test.Custom.DynamicCharacterNode
import           Test.QuickCheck


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Left          _ -> performCounterExampleSearch
      Right (lhs,rhs) -> performImplementationComparison lhs rhs


parseArgs :: [String] -> Either String (String, String)
parseArgs args =
  case args of
    []          -> Left "No arguments supplied!"
    [_]         -> Left "Only one argument supplied, expecting two sequences."
    arg1:arg2:_ -> Right (arg1, arg2)


performCounterExampleSearch :: IO ()
performCounterExampleSearch = do
    putStrLn "Performing stocastic counter-example search:"
    quickCheckWith stdArgs { maxSuccess = 10000 } counterExampleCheck


counterExampleCheck :: DynamicCharacterNode -> Bool
counterExampleCheck node = value == value
  where
    value = getDynamicCharacteracterDecoration node ^. finalUngapped


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = do
    putStrLn "Attempting construction:"
    let !value = constructNode (readSequence lhs) (readSequence rhs)
    print $ show value
    pure ()
  where
    alphabet = fromSymbols ["A","C","G","T"]
--    readSequence :: String -> DynamicCharacter
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
