{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Map                     as M
import           Data.MemoTrie                (memo)
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Validation
import qualified Data.Vector                  as V
import           Numeric.Natural
import           Options.Applicative
import           System.Random
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


data Tree where
   Leaf :: Tree
   Node :: Tree -> Tree -> Tree
   deriving (Show, Eq, Ord)


data  Enumeration a = Enumeration
    { fromNat  :: Natural -> a
    , enumSize :: Natural
    } deriving (Functor)


instance Applicative Enumeration where

    pure    = singleEnum

    f <*> x = uncurry ($) <$> (f >< x)


instance Monad Enumeration where

    return  = pure

    e >>= f = fold1 $ f <$> enumerate e


instance Semigroup (Enumeration a) where

    e1 <> e2 = Enumeration
      (\n -> if n < enumSize e1 then fromNat e1 n else fromNat e2 (n - enumSize e1))
      (enumSize e1 + enumSize e2)


instance Random Natural where

    random g
      | s < 0     = (fromIntegral $ abs i, g')
      | s > 0     = (fromIntegral i      , g')
      | otherwise = (0                   , g')
      where
        (i,g') = random g
        s      = signum (i :: Integer)

    randomR (x,y) g
      | s < 0     = (fromIntegral $ abs i, g')
      | s > 0     = (fromIntegral i      , g')
      | otherwise = (0                   , g')
      where
        (i,g') = randomR (toInteger x, toInteger y) g
        s      = signum (i :: Integer)


main :: IO ()
main = do
    userInput <- parseUserInput
    case toEither $ validateUserInput userInput of
      Left errors -> putStrLn . unlines $ toList errors
      Right spec  -> do
        labledTree    <- generateRandomTree $ specifiedLeaves spec
        decoratedTree <- generateRandomSequence
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
    validProbability p
      | 0 <= p && p < 1 = Just p
      | otherwise       = Nothing

generateRandomSequence
  :: Set String             -- ^ Alphabet
  -> Double                 -- ^ Substitution probablity (0,1)
  -> Maybe (Double, Double) -- ^ (Insertion, Deletion)    probablity (0,1)
  -> Word                   -- ^ Root sequence length
  -> BinaryTree a b
  -> IO (BinaryTree [String] b)
generateRandomSequence alphabet sub indelMay rootLen tree = do
  rootSequence <- replicateM (fromEnum rootLen) randomSymbol
  case tree of
    Terminal _ x -> pure $ Terminal rootSequence x
    Branch _ l r -> Branch rootSequence
                      <$> fromParent rootSequence l
                      <*> fromParent rootSequence r
  where
    alphaSize    = length alphabet
    alphaVec     = V.fromListN alphaSize $ toList alphabet
    randomSymbol = (alphaVec V.!) <$> randomRIO (0, alphaSize - 1)

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
          subV <- randomRIO (0,1) :: IO Double
          x'   <- if   subV <= sub
                  then randomSymbol
                  else pure x
          -- Check if we are insert/deleting
          case indelMay of
            Nothing        -> pure [x']
            Just (ins,del) -> do
              -- Do we delete the symbol
              delV <- randomRIO (0,1) :: IO Double
              let delStr = if   delV <= del
                           then []
                           else [x']
              -- Do we add an inserted prefix string
              (<> delStr) <$> insertStr ins

        insertStr :: Double -> IO [String]
        insertStr ins = do
            insV <- randomRIO (0,1) :: IO Double
            if insV <= ins
            then (:) <$> randomSymbol <*> insertStr ins
            else pure []


generateRandomTree :: Set String -> IO (BinaryTree () String)
generateRandomTree leafLabels = do
    xs        <- shuffleM $ toList leafLabels
    treeIndex <- randomRIO (0, catalanNum leafCount - 1) :: IO Natural
    pure $ getLabledTreeFromIndex xs treeIndex
  where
    leafCount    = length leafLabels

    catalanNum n = head . drop (n-1) $ scanl (\c x -> c*2*(2*x-1) `div` (x+1)) 1 [1..]

    addLabels :: (Monoid m) => a -> State [m] m
    addLabels = const $ do
      x <- get
      case x of
        []   -> pure mempty
        e:es -> do
          put  es
          pure e

    getLabledTreeFromIndex :: [String] -> Natural -> BinaryTree () String
    getLabledTreeFromIndex leaves treeIndex = labeledTree
      where
        treeTopology  = fromNat (enumTreesMemo (length leaves - 1)) treeIndex :: Tree
        unlabeledTree = toBinaryTree treeTopology :: BinaryTree () ()
        labeledTree   = (`evalState` leaves) $ traverse addLabels unlabeledTree



enumerate :: Enumeration a -> NonEmpty a
enumerate (Enumeration f n) = fmap f $ 0 :| [1 .. n-1]


enumTreesMemo :: Int -> Enumeration Tree
enumTreesMemo = memo enumTreesMemo'
  where
    enumTreesMemo' 0 = singleEnum Leaf
    enumTreesMemo' n = fold1 $
        ( Node <$> enumTreesMemo (n-1) <*> enumTreesMemo 0
        ) :|
        [ Node <$> enumTreesMemo (n-k-1) <*> enumTreesMemo k
        | k <- [1 .. n-1]
        ]


singleEnum :: a -> Enumeration a
singleEnum a = Enumeration (const a) 1


-- = fromNat (enumTreesMemo 1000) 8234587623904872309875907638475639485792863458726398487590287348957628934765


(><) :: Enumeration a -> Enumeration b -> Enumeration (a,b)
(><) e1 e2 = Enumeration
    (\n -> let (l,r) = n `divMod` enumSize e2 in (fromNat e1 l, fromNat e2 r))
    (enumSize e1 * enumSize e2)


toBinaryTree :: Tree -> BinaryTree () ()
toBinaryTree Leaf       = Terminal () ()
toBinaryTree (Node l r) = Branch () (toBinaryTree l) (toBinaryTree r)


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


toParens :: Tree -> String
toParens Leaf       = ""
toParens (Node l r) = fold ["(", toParens l, ")", toParens r]
