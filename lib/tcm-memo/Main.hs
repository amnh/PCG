{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Bio.Character.Exportable.Class
import           Control.Applicative            (liftA2)
import           Data.TCM.Memoized.FFI
import           Foreign.C.Types
import           Safe                           (readMay)
import           System.Environment             (getArgs)


newtype MyStruct = T [CULong] deriving (Show)


instance Exportable MyStruct where

    toExportableBuffer (T xs) =
        ExportableCharacterSequence
        { exportedElementCountSequence = 5
        , exportedElementWidthSequence = toEnum $ length xs
        , exportedBufferChunks         = xs
        }

    fromExportableBuffer   = T . exportedBufferChunks
    toExportableElements   = undefined
    fromExportableElements = undefined


main :: IO ()
main = do
   args <- getArgs
   case parseArgs args of
     Left  argsError  -> print argsError
     Right (lhs, rhs) ->
       let (result, cost) = getMedianAndCost2D linearNormTCM (T [lhs]) (T [rhs])
       in  do
           putStrLn "Cost:"
           print cost
           putStrLn "Result:"
           print result


linearNormTCM :: MemoizedCostMatrix
linearNormTCM = getMemoizedCostMatrix 5 linearNorm
  where
    linearNorm i j = max i j - min i j


parseArgs :: [String] -> Either String (CULong, CULong)
parseArgs args =
  case args of
    []  -> Left "No arguments supplied!"
    [_] -> Left "Only one argument was supplied, expecting two arguments."
    x:y:_ ->
      case liftA2 (,) (readMay x) (readMay y) of
        Nothing       -> Left "An invalid input was supplied. Expecting two positive integers."
        Just r@(lhs, rhs)
          | lhs > 31  -> Left "First argument is above 31!"
          | rhs > 31  -> Left "Second argument is above 31!"
          | otherwise -> Right r


