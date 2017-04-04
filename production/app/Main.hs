module Main (main) where


import Control.Evaluation
--import Data.Functor ((<$))
import PCG.Computation.Internal
import PCG.Script
--import System.Environment (getContents)
import System.IO
import Text.Megaparsec


--main = undefined
{--}
main :: IO ()
main = hSetBuffering stdout NoBuffering
   >>  getContents
   >>= checkInput . parse' scriptStreamParser "STDIN stream"
   where
     checkInput (Left  err) = putStrLn $ parseErrorPretty err
     checkInput (Right val) = --print =<< runEvaluation (evaluate =<< (state . evalEither . interpret) val)
                              renderSearchState =<< runEvaluation (evaluate =<< (state . evalEither . interpret) val)
     parse' :: Parsec Dec s a -> String -> s -> Either (ParseError (Token s) Dec) a
     parse' = parse
      
