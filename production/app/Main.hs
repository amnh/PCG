{-# LANGUAGE FlexibleContexts #-}

module Main (main) where


import Control.Evaluation
--import Data.Functor ((<$))
import Data.Void
import PCG.Computation.Internal
import PCG.Syntax
--import System.Environment (getContents)
import System.IO
import Text.Megaparsec


--main = undefined
{--}
main :: IO ()
main = hSetBuffering stdout NoBuffering
   >>  getContents
   >>= (\str -> checkInput str $ parse' computationalStreamParser "STDIN stream" str)
   where
     checkInput str (Left  err) = putStrLn $ parseErrorPretty' str err
     checkInput _   (Right val) = renderSearchState =<< runEvaluation (evaluate (optimizeComputation val))

     parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
     parse' = parse
      
