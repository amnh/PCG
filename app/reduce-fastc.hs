{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Data.Foldable
import Data.Key
import Data.List.NonEmpty           (NonEmpty (..))
import Data.Map                     (Map, findMin, singleton)
import Data.Scientific
import Data.Semigroup.Foldable
import Data.Text.Short              (toString)
import Data.Validation
import Data.Vector.NonEmpty         (Vector)
import Data.Void
import File.Format.Fastc
import Options.Applicative
import Text.Megaparsec              (ParseErrorBundle, Stream, Token, parse)
import Text.Megaparsec.Error        (errorBundlePretty)
import Text.PrettyPrint.ANSI.Leijen (string)


newtype Specification = Specification { specifiedRatio :: Rational }
    deriving stock (Eq, Show)


newtype UserInput = UserInput { inputRatio :: Scientific }
    deriving stock (Eq, Show)


main :: IO ()
main = do
    userInput <- parseUserInput
    case toEither $ validateUserInput userInput of
      Left errors -> putStrLn . unlines $ toList errors
      Right spec  -> do
        stream <- getContents
        case parseStream stream of
          Left parseError -> putStrLn $ errorBundlePretty parseError
          Right seqs      -> putStrLn . renderSequences $ reduceSequences (specifiedRatio spec) seqs


parseStream :: (Stream s, Token s ~ Char) => s -> Either (ParseErrorBundle s Void) FastcParseResult
parseStream = parse fastcStreamParser "STDIN"


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
      UserInput
        <$> argSpec 'r' "ratio" "Ratio of symbols to retain :: [0, 1]"

    argSpec :: Read a => Char -> String -> String -> Parser a
    argSpec c s h = option auto $ fold [short c, long s, help h]

    description = fold
        [ fullDesc
        , headerDoc . Just $ string "\n  Reduce the symbols of FASTC file by truncating from the ends of the sequence"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]


validateUserInput :: UserInput -> Validation (NonEmpty String) Specification
validateUserInput userInput =
    Specification
      <$> validate (pure "ratio is outside rang [0, 1]") validRatio (inputRatio userInput)
  where
    validRatio :: Scientific -> Maybe Rational
    validRatio x =
        let r = toRational x
        in  if r < 0 || 1 < r
            then Nothing
            else Just r


reduceSequences :: Rational -> FastcParseResult -> Map Identifier [Vector Symbol]
reduceSequences ratio seqs =
    let m = buildMap seqs
        n = ceiling . (* ratio) . fromIntegral . length . snd $ findMin m
    in  cutLength n <$> m
  where
    buildMap :: FastcParseResult -> Map Identifier [Vector Symbol]
    buildMap = foldMap (singleton <$> fastcLabel <*> toList . fastcSymbols)

    cutLength :: Int -> [Vector Symbol] -> [Vector Symbol]
    cutLength newLength taxSeq =
        let !midPoint   = length taxSeq `div` 2
            !startPoint = midPoint - (newLength `div` 2)
        in  take newLength . drop startPoint $ toList taxSeq


renderSequences :: Map Identifier [Vector Symbol] -> String
renderSequences = foldMapWithKey $ \k v -> fold ["> ", toString k, "\n", showSequence v, "\n"]


showSequence :: [Vector Symbol] -> String
showSequence xs
  | any (\e -> length e > 1) shownElems = unwords shownElems
  | otherwise = fold shownElems
  where
    shownElems = showSequenceElement <$> toList xs


showSequenceElement :: Vector Symbol -> String
showSequenceElement element =
    case toString <$> toNonEmpty element of
      x:|[] -> x
      x:|xs -> "[ " <> unwords (x:xs) <> " ]"

