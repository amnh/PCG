module Main (main) where

--import Data.Bifunctor
import Control.Evaluation
import PCG.Computation.Internal
import PCG.Script
import Text.Megaparsec
--main = undefined
{--}
main :: IO ()
main = getContents
   >>= checkInput . parse scriptStreamParser "STDIN stream"
   where
     checkInput (Left  err) = print err
     checkInput (Right val) = runEvaluation (evaluate =<< (state . evalEither . interpret) val)
                          >>= print
{--}
