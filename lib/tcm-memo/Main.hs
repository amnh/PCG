{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import Bio.Character.Exportable
import Control.Applicative      (liftA2)
import Data.TCM.Memoized
import Foreign.C.Types
import Safe                     (readMay)
import System.Environment       (getArgs)


newtype MyStruct = T [CULong]
  deriving stock (Show)


instance ExportableBuffer MyStruct where

    toExportableBuffer (T xs) =
        ExportableCharacterBuffer
        { exportedElementCountBuffer = 5
        , exportedElementWidthBuffer = toEnum $ length xs
        , exportedBufferChunks         = xs
        }

    fromExportableBuffer   = T . exportedBufferChunks


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
linearNormTCM = generateMemoizedTransitionCostMatrix {-getMemoizedCostMatrix-} 5 linearNorm
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


