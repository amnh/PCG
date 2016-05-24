module Main (main) where

import Text.Megaparsec
--import Data.Functor ((<$))
import PCG.Computation.Internal
import Control.Evaluation
import PCG.Script

--import System.Environment (getContents)
--main = undefined
{--}
main :: IO ()
main = getContents
   >>= checkInput . parse' scriptStreamParser "STDIN stream"
   where
     checkInput (Left  err) = putStrLn $ parseErrorPretty err
     checkInput (Right val) = --print =<< runEvaluation (evaluate =<< (state . evalEither . interpret) val)
                              renderSearchState =<< runEvaluation (evaluate =<< (state . evalEither . interpret) val)
     parse' :: Parsec Dec s a -> String -> s -> Either (ParseError (Token s) Dec) a
     parse' = parse
      
