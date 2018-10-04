{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Arrow             ((***))
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable
import Data.Key
import Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.Map    as M
import Data.Matrix.NotStupid hiding (toList)
import Data.MemoTrie             (memo)
import Data.Semigroup        hiding (option)
import Data.Set                  (Set)
import qualified Data.Set    as S
import Data.Tuple                (swap)
import Data.Validation
import qualified Data.Vector as V
import Options.Applicative
import Numeric.Natural
import Prelude hiding (zipWith)
import Text.PrettyPrint.ANSI.Leijen (string)
import System.Random
import System.Random.Shuffle


data  MetricSpecification
    = MetricSpecification
    | AllMetrics
    | Spec Metric


data  Metric
    = Discrete
    | LinearNorm1
    | Sub1Gap2
    | Sub2Gap1
    | HammingDistance
    | Leveshtein


data  Specification
    = Specification
    { specifiedAlphabet   :: Set String
    , specifiedOutputFile :: FilePath
    , specifiedMetric     :: MetricSpecification
    }


data  UserInput
    = UserInput
    { inputAlphabet      :: [String]
    , inputOutputFile    :: FilePath
    , inputMetric        :: MetricSpecification
    }


main :: IO ()
main = do
    userInput <- parseUserInput
    let specification = validateUserInput   userInput
    let alphabet      = specifiedAlphabet   specification
    let filePath      = specifiedOutputFile specification
    case specifiedMetric specification of
      Spec metric -> generateFile alphabet metric filePath
      AllMetrics  -> mapM_
          ( uncurry (generateFile alphabet)
          . fmap (insertSuffix filePath)
          ) [ (Discrete       , "discrete"  )
            , (LinearNorm1    , "L1-norm"   )
            , (Sub1Gap2       , "1-2"       )
            , (Sub2Gap1       , "2-1"       )
            , (HammingDistance, "hamming"   )
            , (Leveshtein     , "leveshtein")
            ]


insertSuffix :: FilePath -> String -> FilePath
insertSuffix path suff = mconcat [ prefixName, "-", suff, ".", extension ]
  where
    (prefixName, extension) = swap . (reverse *** reverse . tail') . break (=='.') $ reverse path
    tail'    []  = []
    tail' (_:xs) = xs


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
      UserInput
        <$> argSpec 'a' "alphabet"  "List of symbols in the alphabet :: [String]"
        <*> argStr  'o' "output"    "Output file path for TCM        :: FilePath"
        <*> asum [ metricFlag (AllMetrics          ) "all"        "All metrics listed below"
                 , metricFlag (Spec Discrete       ) "discrete"   "Discrete metric"
                 , metricFlag (Spec LinearNorm1    ) "L1-norm"    "1st linear norm"
                 , metricFlag (Spec Sub1Gap2       ) "1-2"        "1 substitution, 2 gap"
                 , metricFlag (Spec Sub2Gap1       ) "2-1"        "2 substitution, 1 gap"
                 , metricFlag (Spec HammingDistance) "hamming"    "Hamming distance, treating symbols as strings"
                 , metricFlag (Spec Leveshtein     ) "leveshtein" "Leveshtein distance, treating symbols as strings"
                 ]

    argSpec :: Read a => Char -> String -> String -> Parser a
    argSpec c s h = option auto $ mconcat [short c, long s, help h]

    argStr :: Char -> String -> String -> Parser String
    argStr c s h = strOption $ mconcat [short c, long s, help h]

    metricFlag val str desc = flag' val $ mconcat [long str, help desc]

    description = mconcat
        [ fullDesc
        , headerDoc . Just $ string "\n  Generate a TCM specification for a given alphabet"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]


validateUserInput :: UserInput -> Specification
validateUserInput =
    Specification
      <$> S.fromList . inputAlphabet
      <*> inputOutputFile
      <*> inputMetric


generateFile :: Set String -> Metric -> FilePath -> IO ()
generateFile alphabet metric path = writeFile path tcmFile
  where
    tcmFile = renderTCM alphabet tcm
    tcm     = tcmGen alphabet
    tcmGen  = case metric of
                Discrete        -> defineDiscrete
                LinearNorm1     -> defineL1Norm
                Sub1Gap2        -> defineOneTwo
                Sub2Gap1        -> defineTwoOne
                HammingDistance -> defineHammingDistance
                Leveshtein      -> defineLeveshtein


renderTCM :: Set String -> Matrix Word -> String
renderTCM alphabet tcmVals = unlines . fmap unwords $ shownAlphabet : shownMatrix
  where
    shownAlphabet = pad <$> symbolVals
    shownMatrix   = toRows $ pad <$> valueVals
    symbolVals    = toList alphabet
    valueVals     = show <$> tcmVals
    maxWidth      = max (getMaxLen symbolVals) (getMaxLen valueVals)
    getMaxLen :: (Foldable f, Foldable t, Functor f) => f (t a) -> Int
    getMaxLen     = maximum . fmap length 
    pad str       = replicate (maxWidth - length str) ' ' <> str
    toRows m      = toList . (`getRow` m) <$> [0 .. nrows m - 1]


defineDiscrete :: Set String -> Matrix Word
defineDiscrete alphabet = matrix (len + 1) (len + 1)
                        $ \(i,j) -> if i == j then 0 else 1
  where
    len = length alphabet


defineL1Norm :: Set String -> Matrix Word
defineL1Norm alphabet = matrix (len + 1) (len + 1)
                      $ \(i,j) -> toEnum $ max i j - min i j
  where
    len = length alphabet


defineOneTwo :: Set String -> Matrix Word
defineOneTwo alphabet =
    let len = length alphabet
    in  matrix (len + 1) (len + 1)
        $ \(i,j) -> if i == j
                    then 0
                    else if i == len || j == len
                    then 2
                    else 1


defineTwoOne :: Set String -> Matrix Word
defineTwoOne alphabet =
    let len = length alphabet
    in  matrix (len + 1) (len + 1)
        $ \(i,j) -> if i == j
                    then 0
                    else if i == len || j == len
                    then 1
                    else 2


defineHammingDistance :: Set String -> Matrix Word
defineHammingDistance alphabet = mat
  where
    len = length alphabet
    vec = V.fromListN len $ toList alphabet
    gap = (2*) . maximum $ (mat !) <$> [(i,j) | i <- [0 .. len - 1], j <- [0 .. len - 1]]
    mat = matrix (len + 1) (len + 1)
        $ \(i,j) -> if   i == len || j == len
                    then if i == j
                         then 0
                         else gap
                    else let x = vec V.! i
                             y = vec V.! j
                             d = max (length x) (length y) -
                                 min (length x) (length y)
                             s = sum $ zipWith (\a b -> if a == b then 0 else 1) x y
                         in  toEnum $ d + s

      
defineLeveshtein :: Set String -> Matrix Word
defineLeveshtein alphabet = mat
  where
    len = length alphabet
    vec = V.fromListN len $ toList alphabet
    gap = (2*) . maximum $ (mat !) <$> [(i,j) | i <- [0 .. len - 1], j <- [0 .. len - 1]]
    mat = matrix (len + 1) (len + 1)
        $ \(i,j) -> if   i == len || j == len
                    then if i == j
                         then 0
                         else gap
                    else let x = vec V.! i
                             y = vec V.! j
                             m = length x
                             n = length y
                             z = matrix (m) (n)
                               $ \(i,j) -> if   min i j == 0
                                           then max i j
                                           else minimum
                                                  [ z ! (i-1, j  ) + 1
                                                  , z ! (i  , j-1) + 1
                                                  , z ! (i-1, j-1) + if x !! i /= y !! j then 1 else 0
                                                  ]
                         in  toEnum $ z ! (m-1, n-1)
