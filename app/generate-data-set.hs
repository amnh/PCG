{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Map                     as M
import           Data.MemoTrie                (memo)
import           Data.Semigroup.Foldable
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Validation
import qualified Data.Vector                  as V
import           GHC.Natural
import           Options.Applicative
import           System.Random
import           System.Random.MWC
import           System.Random.Shuffle
import           Text.PrettyPrint.ANSI.Leijen (string)


data  Specification
    = Specification
    { specifiedAlphabet     :: Set String
    , specifiedLeaves       :: Set String
    , specifiedFASTA        :: FilePath
    , specifiedNewick       :: FilePath
    , specifiedInsertion    :: Double
    , specifiedDeletion     :: Double
    , specifiedSubstitution :: Double
    , specifiedRootLength   :: Word
    , specifiedAlignedData  :: Bool
    } deriving (Eq, Show)


data  UserInput
    = UserInput
    { inputAlphabet     :: [String]
    , inputLeaves       :: [String]
    , inputFASTA        :: FilePath
    , inputNewick       :: FilePath
    , inputInsertion    :: Double
    , inputDeletion     :: Double
    , inputSubstitution :: Double
    , inputRootLength   :: Word
    , inputAlignedData  :: Bool
    } deriving (Eq, Show)


data  BinaryTree b a
    = Branch b (BinaryTree b a) (BinaryTree b a)
    | Terminal b a
    deriving (Eq, Functor, Foldable, Ord, Show, Traversable)


main :: IO ()
main = do
    userInput <- parseUserInput
    case toEither $ validateUserInput userInput of
      Left errors -> putStrLn . unlines $ toList errors
      Right spec  -> do
        png <- createSystemRandom
        labledTree    <- generateRandomTree png $ specifiedLeaves spec
        decoratedTree <- generateRandomSequence png
                           <$> specifiedAlphabet
                           <*> specifiedSubstitution
                           <*> getInDelContext
                           <*> specifiedRootLength
                           <*> const labledTree
                           $ spec
        writeFile (specifiedNewick spec) $ toNewick decoratedTree
        writeFile (specifiedFASTA  spec) $ toFASTA  decoratedTree


getInDelContext :: Specification -> Maybe (Double, Double)
getInDelContext userInput
  | specifiedAlignedData userInput = Nothing
  | otherwise = Just $ ((,) <$> specifiedInsertion <*> specifiedDeletion) userInput


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
      UserInput
        <$> argSpec 'a' "alphabet"     "List of symbols in the alphabet  :: [String]"
        <*> argSpec 'l' "leaves"       "List of leave node identifiers   :: [String]"
        <*> argStr  'f' "fasta"        "FASTA  data output file          :: FilePath"
        <*> argStr  'n' "newick"       "Newick tree output file          :: FilePath"
        <*> argSpec 'i' "insert"       "Insertion    event probability   :: Double (0, 1)"
        <*> argSpec 'd' "delete"       "Deletion     event probability   :: Double (0, 1)"
        <*> argSpec 's' "substitution" "Substitution event probability   :: Double (0, 1)"
        <*> argSpec 'r' "root-length"  "Sequence length at the root node :: Word"
        <*> switch  (fold [long "aligned", help "Generate aligned output data"])

    argSpec :: Read a => Char -> String -> String -> Parser a
    argSpec c s h = option auto $ fold [short c, long s, help h]

    argStr :: Char -> String -> String -> Parser String
    argStr c s h = strOption $ fold [short c, long s, help h]

    description = fold
        [ fullDesc
        , headerDoc . Just $ string "\n  Generate a random Newick tree and FASTA file of random sequences"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]


validateUserInput :: UserInput -> Validation (NonEmpty String) Specification
validateUserInput userInput =
    Specification
      <$> (pure . S.fromList . inputAlphabet) userInput
      <*> (pure . S.fromList . inputLeaves)   userInput
      <*> (pure . inputFASTA ) userInput
      <*> (pure . inputNewick) userInput
      <*> validate (pure "insertion probability outside range (0, 1)")    validProbability (inputInsertion    userInput)
      <*> validate (pure "deletion probability outside range (0, 1)")     validProbability (inputDeletion     userInput)
      <*> validate (pure "substitution probability outside range (0, 1)") validProbability (inputSubstitution userInput)
      <*> (pure . inputRootLength ) userInput
      <*> (pure . inputAlignedData) userInput
  where
    validProbability x
      | 0 <= x && x < 1 = Just x
      | otherwise       = Nothing


generateRandomSequence
  :: GenIO
  -> Set String             -- ^ Alphabet
  -> Double                 -- ^ Substitution probablity (0,1)
  -> Maybe (Double, Double) -- ^ (Insertion, Deletion)    probablity (0,1)
  -> Word                   -- ^ Root sequence length
  -> BinaryTree a b
  -> IO (BinaryTree [String] b)
generateRandomSequence png alphabet sub indelMay rootLen tree = do
  rootSequence <- replicateM (fromEnum rootLen) randomSymbol
  case tree of
    Terminal _ x -> pure $ Terminal rootSequence x
    Branch _ l r -> Branch rootSequence
                      <$> fromParent rootSequence l
                      <*> fromParent rootSequence r
  where
    alphaSize    = length alphabet
    alphaVec     = V.fromListN alphaSize $ toList alphabet
    randomSymbol = (alphaVec V.!) <$> uniformR (0, alphaSize - 1) png

    fromParent :: [String] -> BinaryTree a b -> IO (BinaryTree [String] b)
    fromParent pStr bTree = do
        mStr <- mutateString pStr
        case bTree of
          Terminal _ x -> pure $ Terminal mStr x
          Branch _ l r -> Branch mStr <$> fromParent mStr l <*> fromParent mStr r

    mutateString :: [String] -> IO [String]
    mutateString xs = do
        mStr <- mapM mutateSymbol xs
        suff <- case indelMay of
                  Nothing      -> pure []
                  Just (ins,_) -> insertStr ins
        pure  $ fold mStr <> suff
      where
        mutateSymbol :: String -> IO [String]
        mutateSymbol x = do
          -- Do we substitute the symbol
          subV <- uniform png :: IO Double
          x'   <- if   subV <= sub
                  then randomSymbol
                  else pure x
          -- Check if we are insert/deleting
          case indelMay of
            Nothing        -> pure [x']
            Just (ins,del) -> do
              -- Do we delete the symbol
              delV <- uniform png :: IO Double
              let delStr = if   delV <= del
                           then []
                           else [x']
              -- Do we add an inserted prefix string
              (<> delStr) <$> insertStr ins

        insertStr :: Double -> IO [String]
        insertStr ins = do
            insV <- uniform png :: IO Double
            if insV <= ins
            then (:) <$> randomSymbol <*> insertStr ins
            else pure []


generateRandomTree :: GenIO -> Set String -> IO (BinaryTree () String)
generateRandomTree png leafLabels = do
    xs <- Seq.fromList <$> shuffleM (toList leafLabels)
    genSubtree xs
  where
    genSubtree :: Seq String -> IO (BinaryTree () String)
    genSubtree leaves =
        case length leaves of
          1 -> pure $ Terminal () $ Seq.index leaves 0
          leafCount -> do
              leftSubtreeSize <- uniformR (1, leafCount - 1) png
              let (leftLeaves, rightLeaves) = Seq.splitAt leftSubtreeSize leaves
              Branch () <$> genSubtree leftLeaves <*> genSubtree rightLeaves


toNewick :: BinaryTree a String -> String
toNewick = (<>";\n") . go
  where
    go (Terminal _ x) = x
    go (Branch _ l r) = fold ["(", go l, ",", go r, ")"]


toFASTA :: BinaryTree [String] String -> String
toFASTA = foldMapWithKey f . buildMap
  where
    buildMap (Terminal s i) = M.singleton i $ unwords s
    buildMap (Branch _ l r) = buildMap l <> buildMap r

    f k v = fold [ ">", k, "\n", v, "\n\n" ]
