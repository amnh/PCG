{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Arrow                ((***))
import           Control.Monad
import           Data.Foldable
import           Data.Foldable.Custom         (sum')
import           Data.Key
import           Data.Matrix.NotStupid        hiding (toList)
import           Data.Semigroup               hiding (option)
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Tuple                   (swap)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           Options.Applicative
import           Prelude                      hiding (zipWith)
import           Text.PrettyPrint.ANSI.Leijen (string)


data  MetricSpecification
    = AllMetrics
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
    { inputAlphabet   :: [String]
    , inputOutputFile :: FilePath
    , inputMetric     :: MetricSpecification
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
insertSuffix path suff = fold [ prefixName, "-", suff, ".", extension ]
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
        <*> asum [ metricFlag  AllMetrics            "all"        "All metrics listed below"
                 , metricFlag (Spec Discrete       ) "discrete"   "Discrete metric"
                 , metricFlag (Spec LinearNorm1    ) "L1-norm"    "1st linear norm"
                 , metricFlag (Spec Sub1Gap2       ) "1-2"        "1 substitution, 2 gap"
                 , metricFlag (Spec Sub2Gap1       ) "2-1"        "2 substitution, 1 gap"
                 , metricFlag (Spec HammingDistance) "hamming"    "Hamming distance, treating symbols as strings"
                 , metricFlag (Spec Leveshtein     ) "leveshtein" "Leveshtein distance, treating symbols as strings"
                 ]

    argSpec :: Read a => Char -> String -> String -> Parser a
    argSpec c s h = option auto $ fold [short c, long s, help h]

    argStr :: Char -> String -> String -> Parser String
    argStr c s h = strOption $ fold [short c, long s, help h]

    metricFlag v s desc = flag' v $ fold [long s, help desc]

    description = fold
        [ fullDesc
        , headerDoc . Just $ string "\n  Generate a TCM specification for a given alphabet"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]


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
    pad xs        = replicate (maxWidth - length xs) ' ' <> xs
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
    (len,vec,gap) = getSymbolContext alphabet mat
    mat = matrix (len + 1) (len + 1)
        $ \(i,j) -> if   i == len || j == len
                    then if i == j
                         then 0
                         else gap
                    else let x = vec V.! i
                             y = vec V.! j
                             d = max (length x) (length y) -
                                 min (length x) (length y)
                             s = sum' $ zipWith (\a b -> if a == b then 0 else 1) x y
                         in  toEnum $ d + s


getSymbolContext :: Set String -> Matrix Word -> (Int, Vector String, Word)
getSymbolContext alphabet mat = (len, vec, gap)
  where
    len = length alphabet
    vec = V.fromListN len $ toList alphabet
    gap = getGapCost mat


getGapCost :: Matrix Word -> Word
getGapCost mat = (2*) . maximum $ (mat !) <$>
    [ (i,j)
    | i <- [0 .. nrows mat - 2]
    , j <- [0 .. ncols mat - 2]
    ]


defineLeveshtein :: Set String -> Matrix Word
defineLeveshtein alphabet = mat
  where
    (len,vec,gap) = getSymbolContext alphabet mat
    mat = matrix (len + 1) (len + 1)
        $ \(i,j) -> if   i == len || j == len
                    then if i == j
                         then 0
                         else gap
                    else let x = vec V.! i
                             y = vec V.! j
                             m = length x
                             n = length y
                             z = matrix m n
                               $ \(a,b) -> if   min a b == 0
                                           then max a b
                                           else minimum
                                                  [ z ! (a-1, b  ) + 1
                                                  , z ! (a  , b-1) + 1
                                                  , z ! (a-1, b-1) + if x !! a /= y !! b then 1 else 0
                                                  ]
                         in  toEnum $ z ! (m-1, n-1)
