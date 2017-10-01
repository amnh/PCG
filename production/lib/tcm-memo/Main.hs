{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Bio.Character.Exportable.Class
import Control.Applicative (liftA2)
import Data.TCM.Memoized.FFI
import Foreign.C.Types
import Safe                (readMay)
import System.Environment  (getArgs)


data MyStruct = T [CULong] deriving (Show)


instance Exportable MyStruct where

    toExportableBuffer (T xs) =
        ExportableCharacterSequence
        { exportedElementCountSequence = 5
        , exportedElementWidthSequence = length xs
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
       let (result, cost) = getMedianAndCost linearNormTCM (T [lhs]) (T [rhs])
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
    [_] -> Left "Only one argument was supplied,  expecting two arguments."
    x:y:_ ->
      case liftA2 (,) (readMay x) (readMay y) of
        Nothing           -> Left "An invalid input was supplied. Expecting 2 lists of positive integers."
        Just r@(lhs, rhs) ->
          if      lhs > 31 then Left "First argument is above 31!"
          else if rhs > 31 then Left "Second argument is above 31!"
          else Right r

