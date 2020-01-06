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

import           Control.Monad
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                   as BM
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe
import           Data.Scientific
import           Data.String                  (IsString (fromString))
import           Data.Text.Lazy               (Text, unlines, unwords)
import qualified Data.Text.Lazy               as T
import           Data.Text.Lazy.IO            (putStrLn, writeFile)
import           Data.Validation
import           Numeric.Natural
import           Options.Applicative
import           Prelude                      hiding (putStrLn, unlines, unwords, writeFile)
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen (string)


data  Specification
    = Specification
    { specifiedAlphabet :: Alphabet Text
    , specifiedLength   :: Natural
    , specifiedFASTC    :: FilePath
    }
    deriving stock (Eq, Show)


data  UserInput
    = UserInput
    { inputAlphabet :: [String]
    , inputLength   :: Scientific
    , inputFASTC    :: FilePath
    }
    deriving stock (Eq, Show)


main :: IO ()
main = do
    userInput <- parseUserInput
    case toEither $ validateUserInput userInput of
      Left errors -> putStrLn . unlines $ toList errors
      Right spec  -> do
        randomSeqence <- generateSequence spec
        let fileContent = unlines
              [ fromString $ "> " <> show (specifiedLength spec)
              , showSequence (specifiedAlphabet spec) randomSeqence
              ]
        writeFile (specifiedFASTC spec) fileContent


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
      UserInput
        <$> argSpec 'a' "alphabet" "List of symbols in the alphabet :: [String]"
        <*> argSpec 'l' "length"   "Sequence length                 :: Word"
        <*> argStr  'f' "fastc"    "FASTC data output file          :: FilePath"

    argSpec :: Read a => Char -> String -> String -> Parser a
    argSpec c s h = option auto $ fold [short c, long s, help h]

    argStr :: Char -> String -> String -> Parser String
    argStr c s h = strOption $ fold [short c, long s, help h]

    description = fold
        [ fullDesc
        , headerDoc . Just $ string "\n  Generate a uniformly random sequence of symbols from the alphabet"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]


validateUserInput :: UserInput -> Validation (NonEmpty Text) Specification
validateUserInput userInput =
    Specification
      <$> (pure . fromSymbols . fmap fromString . inputAlphabet) userInput
      <*> validate (pure "length is non-positive") validLength (inputLength userInput)
      <*> (pure . inputFASTC) userInput
  where
    validLength :: Scientific -> Maybe Natural
    validLength x =
        case res of
          Left _          -> Nothing
          Right i | i < 1 -> Nothing
          Right i         -> Just $ fromInteger i
      where
        res :: Either Double Integer
        res = floatingOrInteger x


generateSequence :: Specification -> IO (NonEmpty (NonEmpty Text))
generateSequence spec =
    let !a = length alphabet
        !n = fromEnum $ specifiedLength spec
        !r = (0, 2^a - 2) :: (Integer, Integer)
    in  NE.fromList <$> replicateM n (buildAmbiguityGroup <$> randomRIO r)
  where
    alphabet = specifiedAlphabet spec
    buildAmbiguityGroup nat = NE.fromList $ go bv0
      where
        len = length alphabet
        bv0 = fromNumber (toEnum len) $ nat + 1
        go bv
          | isZeroVector bv = []
          | otherwise = let i = countLeadingZeros bv
                            b = bv `clearBit` i
                        in  alphabet ! i : go b


showSequence :: Alphabet Text -> NonEmpty (NonEmpty Text) -> Text
showSequence alphabet xs
  | any (\e -> T.length e > 1) shownElems = T.unwords shownElems
  | otherwise = fold shownElems
  where
    shownElems = showSequenceElement alphabet <$> toList xs


showSequenceElement :: Alphabet Text -> NonEmpty Text -> Text
showSequenceElement alphabet element = renderAmbiguity $ toIUPAC element
  where
    renderAmbiguity amb =
      case amb of
          x:|[] -> x
          x:|xs -> "[ " <> unwords (x:xs) <> " ]"

    toIUPAC :: NonEmpty Text -> NonEmpty Text
    toIUPAC x
      | isAlphabetDna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToDna
      | isAlphabetRna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToRna
      | isAlphabetAminoAcid alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToAminoAcid
      | otherwise                    = x


{-
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
-}
