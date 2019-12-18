{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Alphabet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Map                     as M
import           Data.Ratio
import           Data.Scientific
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Set                     (Set, singleton)
import qualified Data.Set                     as S
import           Data.String                  (IsString (fromString))
import           Data.Text.Lazy               (Text, intercalate, unlines, unwords)
import qualified Data.Text.Lazy               as T
import           Data.Text.Lazy.Builder       hiding (fromString, singleton)
import           Data.Text.Lazy.IO            (putStrLn, writeFile)
import           Data.Validation
import           Data.Vector.Unboxed          (Vector)
import qualified Data.Vector.Unboxed          as V
import           Data.Word
import           Options.Applicative
import           Prelude                      hiding (putStrLn, unlines, unwords, writeFile)
import           System.Random.MWC
import           System.Random.Shuffle
import           Text.PrettyPrint.ANSI.Leijen (string)
import           TextShow                     (TextShow (showtl))


data  Specification
    = Specification
    { specifiedAlphabet     :: Set Text
    , specifiedLeaves       :: Set Text
    , specifiedFASTA        :: FilePath
    , specifiedTreeFile     :: FilePath
    , specifiedInsertion    :: Ratio Int
    , specifiedDeletion     :: Ratio Int
    , specifiedSubstitution :: Ratio Int
    , specifiedRootLength   :: Word
    , specifiedAlignedData  :: Bool
    , specifiedRenderVER    :: Bool
    }
    deriving stock (Eq, Show)


data  UserInput
    = UserInput
    { inputAlphabet     :: [String]
    , inputLeaves       :: [String]
    , inputFASTA        :: FilePath
    , inputTreeFile     :: FilePath
    , inputInsertion    :: Scientific
    , inputDeletion     :: Scientific
    , inputSubstitution :: Scientific
    , inputRootLength   :: Word
    , inputAlignedData  :: Bool
    , inputRenderVER    :: Bool
    }
    deriving stock (Eq, Show)


data  BinaryTree b a
    = Branch !(BinaryTree b a) !(BinaryTree b a)
    | Terminal !b !a
    deriving stock (Eq, Functor, Foldable, Ord, Show, Traversable)


main :: IO ()
main = do
    userInput <- parseUserInput
    case toEither $ validateUserInput userInput of
      Left errors -> putStrLn . unlines $ toList errors
      Right spec  -> do
        decoratedTree <- generateDecoratedTree spec
        let treeRenderer = if specifiedRenderVER spec then toVER else toNewick
        writeFile (specifiedTreeFile spec) $ treeRenderer decoratedTree
        writeFile (specifiedFASTA    spec) $ toFASTA (specifiedAlphabet spec) decoratedTree


getInDelContext :: Specification -> Maybe (Ratio Int, Ratio Int)
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
        <*> switch  (fold [long "ver"    , help "Render VER instead of newick file"])

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


validateUserInput :: UserInput -> Validation (NonEmpty Text) Specification
validateUserInput userInput =
    Specification
      <$> (pure . S.fromList . fmap fromString . inputAlphabet) userInput
      <*> (pure . S.fromList . fmap fromString . inputLeaves  ) userInput
      <*> (pure . inputFASTA   ) userInput
      <*> (pure . inputTreeFile) userInput
      <*> validate (pure "insertion probability outside range (0, 1)")    validProbability (inputInsertion    userInput)
      <*> validate (pure "deletion probability outside range (0, 1)")     validProbability (inputDeletion     userInput)
      <*> validate (pure "substitution probability outside range (0, 1)") validProbability (inputSubstitution userInput)
      <*> (pure . inputRootLength ) userInput
      <*> (pure . inputAlignedData) userInput
      <*> (pure . inputRenderVER  ) userInput
  where
    validProbability :: Scientific -> Maybe (Ratio Int)
    validProbability x
      | x < 0 || 1 <= x = Nothing
      | num >= bound    = Nothing
      | den >= bound    = Nothing
      | otherwise       = Just $ fromIntegral num % fromIntegral den
      where
        (num, den) = (numerator &&& denominator) $ toRational x
        bound = fromIntegral (maxBound :: Int)


generateDecoratedTree :: Specification -> IO (BinaryTree (Vector Int) Text)
generateDecoratedTree spec = do
    leafSequence <- fmap Seq.fromList . shuffleM . toList $ specifiedLeaves spec
    withSystemRandom . asGenST $ \png -> do
        labledTree <- generateRandomTree png leafSequence
        generateRandomSequence png
          <$> specifiedAlphabet
          <*> specifiedSubstitution
          <*> getInDelContext
          <*> specifiedRootLength
          <*> const labledTree
          $ spec


generateRandomTree :: forall s. GenST s -> Seq Text -> ST s (BinaryTree () Text)
generateRandomTree png = genSubtree
  where
--    genSubtree :: Seq String -> GenST s (BinaryTree () String)
    genSubtree leaves =
        case length leaves of
          1 -> pure . Terminal () $ Seq.index leaves 0
          leafCount -> do
              leftSubtreeSize <- uniformR (1, leafCount - 1) png
              let (leftLeaves, rightLeaves) = Seq.splitAt leftSubtreeSize leaves
              Branch <$> genSubtree leftLeaves <*> genSubtree rightLeaves


generateRandomSequence
  :: forall a b s
  .  GenST s
  -> Set Text                     -- ^ Alphabet
  -> Ratio Int                    -- ^ Substitution probablity (0,1)
  -> Maybe (Ratio Int, Ratio Int) -- ^ (Insertion, Deletion)    probablity (0,1)
  -> Word                         -- ^ Root sequence length
  -> BinaryTree a b
  -> ST s (BinaryTree (Vector Int) b)
generateRandomSequence png alphabet sub indelMay rootLen tree = do
    let !n = fromEnum rootLen
    rootSequence <- V.fromList <$> replicateM n randomSymbol
    withSequence rootSequence tree
  where
    alphaSize    = force $ length alphabet
--    alphaVec     = force . V.fromListN alphaSize $ toList alphabet
    randomSymbol = uniformR (0, alphaSize - 1) png

    withSequence :: Vector Int -> BinaryTree a b -> ST s (BinaryTree (Vector Int) b)
    withSequence currentStr bTree =
        case bTree of
          Terminal _ x -> pure $ Terminal currentStr x
          Branch   l r -> do
            leftStr  <- force <$> mutateString currentStr
            rightStr <- force <$> mutateString currentStr
            Branch <$> withSequence leftStr l <*> withSequence rightStr r

    mutateString :: Vector Int -> ST s (Vector Int)
    mutateString inStr = do
        mStr <- V.foldM' mutateSymbol mempty inStr
        suff <- case indelMay of
                  Nothing      -> pure mempty
                  Just (ins,_) -> insertStr ins
        pure . force . V.fromList . reverse $ suff <> mStr
      where
        mutateSymbol :: [Int] -> Int -> ST s [Int]
        mutateSymbol xs x = do
          -- Do we substitute the symbol
          subV <- uniformR (0, denominator sub - 1) png :: ST s Int
          x'   <- if   subV < numerator sub
                  then randomSymbol
                  else pure x
          -- Check if we are insert/deleting
          case indelMay of
            Nothing        -> pure $ pure x'
            Just (ins,del) -> do
              -- Do we delete the symbol
              delV <- uniformR (0, denominator del - 1) png :: ST s Int
              let delStr = if   delV < numerator del
                           then mempty
                           else pure x'
              -- Do we add an inserted prefix string
              (<> xs <> delStr) <$> insertStr ins

        insertStr :: Ratio Int -> ST s [Int]
        insertStr ins = do
            insV <- uniformR (0, denominator ins - 1) png :: ST s Int
            if insV < numerator ins
            then (:) <$> randomSymbol <*> insertStr ins
            else pure mempty


toNewick :: BinaryTree a Text -> Text
toNewick = (<>";\n") . go
  where
    go (Terminal _ x) = x
    go (Branch   l r) = fold ["(", go l, ",", go r, ")"]


toVER :: BinaryTree a Text -> Text
toVER = renderSets . getSets
  where
    renderSets :: (Set Text, Set (Text, Text), Set Text) -> Text
    renderSets (vSet, eSet, rSet) = unlines
        [ unwords [ {- "vertexset", "=", -} "{", intercalate ", " $       toList vSet, "}"]
        , unwords [ {-   "edgeset", "=", -} "{", intercalate ", " $ f <$> toList eSet, "}"]
        , unwords [ {-   "rootset", "=", -} "{", intercalate ", " $       toList rSet, "}"]
        ]

    f :: (Text, Text) -> Text
    f (x,y) = fold ["(", x, ", ", y, ")"]

    getSets = \case
        Terminal _ x -> (singleton x, mempty, singleton x)
        Branch   l r -> let rSet         = singleton $ showtl (0 :: Word)
                            initialState = (rSet, mempty)
                            (vSet, eSet) = (`evalState` 1) $ go 0 l initialState >>= go 0 r
                        in  (vSet, eSet, rSet)

    go :: Word -> BinaryTree a Text -> (Set Text, Set (Text, Text)) -> State Word (Set Text, Set (Text, Text))
    go pNum (Terminal _ x) (vSet, eSet) = pure (vSet <> singleton x, eSet <> singleton (showtl pNum, x))
    go pNum (Branch   l r) (vSet, eSet) = do
        cNum <- get
        modify succ
        let newSets = (vSet <> singleton (showtl cNum), eSet <> singleton (showtl pNum, showtl cNum))
        go cNum l newSets >>= go cNum r


toFASTA :: Set Text -> BinaryTree (Vector Int) Text -> Text
toFASTA alphabet = foldMapWithKey f . buildMap
  where
    buildMap (Terminal s i) = M.singleton i $ renderer s
    buildMap (Branch   l r) = buildMap l <> buildMap r

    f k v = fold [ ">", k, "\n", v, "\n\n" ]

    !renderer = selectSequenceRenderer alphabet

    selectSequenceRenderer :: Set Text -> Vector Int -> Text
    selectSequenceRenderer inAlphabet
      | anyOf specialAlphabetsAre ourAlphabet = T.pack . V.toList . V.backpermute vec
      | otherwise                             = toLazyText . V.foldl' g mempty
      where
        ourAlphabet = fromSymbols inAlphabet

        specialAlphabetsAre =
            [ isAlphabetAminoAcid
            , isAlphabetDna
            , isAlphabetRna
            , isAlphabetDiscrete
            ]

        vec = V.fromList . fmap T.head $ toList inAlphabet

        g a i = a <> fromLazyText (ourAlphabet ! i) <> " "

        anyOf :: Foldable f => f (a -> Bool) -> a -> Bool
        anyOf fns v = go $ toList fns
          where
            go    []  = False
            go (x:xs) = let !b = x v
                        in  b || go xs
