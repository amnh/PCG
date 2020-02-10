{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           BenchShow                                     hiding (defaultConfig)
import qualified BenchShow                                     as Bench
import           Bio.Character
import           Bio.Character.Encodable
import           Bio.Metadata.Dynamic
import           Control.DeepSeq
import           Criterion.Main                                hiding (defaultConfig)
import qualified Criterion.Main                                as Crit
import           Criterion.Types                               (Config (..))
import           Data.Alphabet
import           Data.Char
import           Data.Either
import           Data.Foldable
import           Data.Key
import           Data.List                                     (intercalate)
import           Data.List.Split                               (splitOn)
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import           Data.Maybe
import           Data.Normalization.Character
import qualified Data.Set                                      as Set
import           Data.String
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           Data.Text.Short                               (ShortText, toString)
import           Data.Void
import           Data.Word
import           File.Format.Fasta                             hiding   (FastaSequenceType (..))
import qualified File.Format.Fasta                             as Fasta (FastaSequenceType (..))
import           Prelude                                       hiding (lookup, zip, zipWith)
import           Text.Megaparsec                               (ParseErrorBundle, errorBundlePretty, parse)
import           Weigh


alphabet :: Alphabet ShortText
alphabet = fromSymbols $ Set.fromList ["A","C","G","T"]


inputPairs :: [((ShortText, String), (ShortText, String))]
inputPairs = [ ( ("a008", "CCGGGAGG")
               , ("r009", "??VBYbBah")
               )
             , ( ("a064", "CAATCAGTAGCCTTCGGTCCAGGCTAACGGGGCGATCCCTTGGTTTGGCCGTCGCATTCCTTTG")
               , ("r072", "VchbbTMaHgNhAhSts-KvBAtYCDWTAtHvhb-MWTSdHKkKADatGmsrWGrktbmKRTBvSmKHAbVM")
               )
             , ( ("a512", "GGTAACCTTTAGCTACCTCCCTACTCTGCGAACCGTATTTGCGCCGCGAGCGCTGGCGTGATTAAGCAGCGTGCAGCTCAAAGGTTGACTCTCCAGATATCCCTTTCGAGCACAGAAATACGACGTGCTATGGTGCGGGAGCTGTGGGAGCTAAAGACCTTTGTCCTTTCTACTTCTAGACATCTCGAGGAGGTAGCCCATCGATGTCGGATACCCCCGTTTCTCTTCGCAAGACCGTGTGTCCCGCAGCGCGCGATCTTGATTTGCTCCGACATACGACCACTTGAGGCCAGATACTGTCACCTCCTAGGCGGAAACGCGGGAAACGTGCGGGGGCACCTTCCAAGGTACCCTTTGACCCTTGCGGCTCTCTGATATTCAGAACCGGGCCAATAAGGGGCCTACTATCTTATCAATAGCATTAACGTTCACGTTAAGGCCCCTGTGGATTCGTGAGCCACGAGACCAGGAAACAATAGCTTGGCCGGAGAAACCTTAGCACTTGATTTAGA")
               , ("r576", "V??sABvBHaADNaW-CHVrTACCB?Hty--KwTAByTgbbRmKchT?SvwtWbV?BsGWTCdB-gakKNgrHNYhrtCTAkwYmrGtyAYS?KWmKN?cv?VrkysswYVYtVgDTgMwYbgMgstVcBsRa-TwatVhBHcGcawkC-CHdyhbc-tcYHRBRMRt-h-sdkYabRScSMkhBH-a-AgAtBmRCTGrhahShvC-mgam-yK-wVDCwmkyCTBwmMGA?CWSdDYSBKSYwmDBCggCgV?KTvgKkNCaghhdddcMVktYc?ahy?DcaytcyrwsDRCCcagHhSAsbraBw-tktNbbgKmHaSb-BSbThAHawyAGwRDkAa-RBshBNr?yRwWRaW?hrwYHA?MyAkddhM?BmsaVWMVT?D?aKATcamvgAvrcAKCrGMrRCdmMDCSSSCyHtgMhATDR?cGTVVHWBrhgGsMVK??Bv-WV?HvwycKH-CwsCNyVMBRKGDARR?syaBvbaDNwtKAhmAvdbtvyGgNmhGasNCcgBvSGHBhyMVDykwBmsmymdytTkyrCHKSSCCSgbvmN-vwaVTtgVTTC-gaMrrKmmWhb")
               )
             ]


outputFile :: String
outputFile = "bench-new-str-align.csv"


main :: IO ()
main =
  let val = traverse makeInputPairs inputPairs
  in  case val of
        Left  err  -> putStrLn $ errorBundlePretty err
        Right strs ->
          let points = foldMap measureStringAlignment strs
          in  do
                benchmarkTime  points
                benchmarkSpace points
                benchmarkRendering


benchmarkTime
  :: [ ( String
       , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
       , (DynamicCharacter, DynamicCharacter)
       )
     ]
  -> IO ()
benchmarkTime points =
    let cfg = Crit.defaultConfig { csvFile = Just outputFile }
        benchTime (label, op, val) = bench label $ nf op val
    in  defaultMainWith cfg $ benchTime <$> points


benchmarkSpace
  :: [ ( String
       , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
       , (DynamicCharacter, DynamicCharacter)
       )
     ]
  -> IO ()
benchmarkSpace points =
    let benchSpace (label, op, val) = Map.singleton label <$> weighFunc op val
    in  do
        mapping <- fold <$> traverse benchSpace points
        content <- filter (/='\r') <$> readFile outputFile
        let (prevLines, newLines) = splitAt 10 . reverse . take 20 . reverse $ lines content
        let updatedLines
              | null newLines =                  appendSpaceInfo mapping <$> prevLines
              | otherwise     = (prevLines <>) $ appendSpaceInfo mapping <$>  newLines
        let str = force $ unlines updatedLines
        -- length and seq to handle lazy I/O
        length content `seq` (writeFile outputFile str)


benchmarkRendering :: IO ()
benchmarkRendering =
    let cfg = Bench.defaultConfig
                { presentation      = Groups PercentDiff
                , selectFields      = filter (flip elem ["time", "mean", "maxrss", "allocatedbytes","livebytes"] . map toLower)
                , classifyBenchmark = \x -> Just ("op", x)
                }
    in  report outputFile Nothing cfg


appendSpaceInfo
  :: Map String (Word64, Word32, Word64, Word64, Word64)
  -> String
  -> String
appendSpaceInfo m csvStr =
    case splitOn "," csvStr of
      []  -> csvStr
      x:_ | x == "Name" -> csvStr <> "," <> headerRowExtension
      x:_ ->
          case x `lookup` m of
            Nothing          -> csvStr
            Just (a,b,c,d,e) -> csvStr <> "," <> intercalate "," [show a, show b, show c, show d, show e]
  where
    headerRowExtension = intercalate ","
      [ "AllocatedBytes"
      , "GCs"
      , "LiveBytes"
      , "MaxBytes"
      , "MaxOSBytes"
      ]


makeInputPairs
  :: ((a, String), (b, String))
  -> Either (ParseErrorBundle String Void) ((a, DynamicCharacter), (b, DynamicCharacter))
makeInputPairs ((a,b), (c,d)) = do
    x <- sequenceStreamParser b
    y <- sequenceStreamParser d
    pure ((a,x), (c,y))


sequenceStreamParser :: String -> Either (ParseErrorBundle String Void) DynamicCharacter
sequenceStreamParser = fmap characterEncoder . parse dnaParser "Embedded-String" . ("> X\n" <>)
  where
    dnaParser = (fastaStreamConverter Fasta.DNA =<< fastaStreamParser)

    characterEncoder = encodeDynamicCharacter . extractFromNormalized . extractFromMap . getNormalizedCharacters

    extractFromMap   = head . toList . snd . Map.findMin

    extractFromNormalized (NormalizedDynamicCharacter x) = x
    extractFromNormalized                             _  = Nothing

    encodeDynamicCharacter = maybe (Missing . toEnum $ length alphabet) (encodeStream alphabet)


measureStringAlignment
  :: ((ShortText, DynamicCharacter), (ShortText, DynamicCharacter))
  -> [( String
      , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
      , (DynamicCharacter, DynamicCharacter)
      )
     ]
measureStringAlignment x = [measureForeignAlignment, measureNewAlignment, measureOldAlignment] <*> [x]


measureOldAlignment
  :: ((ShortText, DynamicCharacter), (ShortText, DynamicCharacter))
  -> ( String
     , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
     , (DynamicCharacter, DynamicCharacter)
     )
measureOldAlignment ((a, lhs), (b, rhs)) = (label, align, (lhs, rhs))
  where
    align (x,y) = ukkonenDO x y $ getMedianAndCost2D memoMatrixValue
    label = fold ["old-", toString a, "-X-", toString b]


measureNewAlignment
  :: ((ShortText, DynamicCharacter), (ShortText, DynamicCharacter))
  -> ( String
     , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
     , (DynamicCharacter, DynamicCharacter)
     )
measureNewAlignment ((a, lhs), (b, rhs)) = (label, align, (lhs, rhs))
  where
    align (x,y) = ukkonenDO x y $ getMedianAndCost2D memoMatrixValue
    label = fold ["new-", toString a, "-X-", toString b]


measureForeignAlignment
  :: ((ShortText, DynamicCharacter), (ShortText, DynamicCharacter))
  -> ( String
     , (DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter)
     , (DynamicCharacter, DynamicCharacter)
     )
measureForeignAlignment ((a, lhs), (b, rhs)) = (label, align, (lhs, rhs))
  where
    align (x,y) = foreignPairwiseDO x y denseMatrixValue
    label = fold ["ffi-", toString a, "-X-", toString b]


costStructure :: (Ord a, Num a) => a -> a -> a
--costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j
costStructure i j
  | i == j    = 0
  | i == 4    = 2
  | j == 4    = 2
  | otherwise = 1


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
