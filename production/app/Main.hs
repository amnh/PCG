module Main (main) where

--import Data.Bifunctor
import Text.Megaparsec
import Data.Functor ((<$))
import PCG.Computation.Internal
import Control.Evaluation
import PCG.Script

--import System.Environment (getContents)
--main = undefined
{--}
main :: IO ()
main = getContents
   >>= checkInput . parse scriptStreamParser "STDIN stream"
   where
     checkInput (Left  err) = print err
     checkInput (Right val) = -- print =<< runEvaluation (evaluate =<< (state . evalEither . interpret) val)
                              () <$ runEvaluation (evaluate =<< (state . evalEither . interpret) val)
       {--}
      
