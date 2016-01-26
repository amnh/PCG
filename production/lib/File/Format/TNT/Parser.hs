{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.List                (intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | The sequence information for a taxon within the TNT file's XREAD command.
-- Contains the 'TaxonName' and the naive 'TaxonSequence' 
type TaxonInfo     = (TaxonName, TaxonSequence)

-- | The name of a taxon in a TNT file's XREAD command.
type TaxonName     = String

-- | The naive sequence of a taxon in a TNT files' XREAD command.
type TaxonSequence = String

-- | Parses an XREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
xreadCommand :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: MonadParsec s m Char => m (Int, Int, NonEmpty TaxonInfo)
    xreadDefinition = uncurry (,,) <$> xreadPreamble <*> xreadSequences <* symbol (char ';')

    xreadValidation :: MonadParsec s m Char => (Int, Int, NonEmpty TaxonInfo) -> m (NonEmpty TaxonInfo)
    xreadValidation (charCount, taxaCount, taxaSeqs)
      | null errors = pure taxaSeqs
      | otherwise   = fails errors
      where
        errors = catMaybes $ [taxaCountError, charCountError]
        taxaCountError = let taxaLength = NE.length taxaSeqs
                         in if taxaCount == taxaLength
                            then Nothing
                            else Just $ concat
                              [ "The number of taxa specified ("
                              , show taxaCount
                              , ") does not match the number of taxa found ("
                              , show $ length taxaSeqs
                              , ")"
                              ]
        charCountError = case NE.filter ((/= charCount) . snd) . fmap (second length) $ taxaSeqs of
                           [] -> Nothing
                           xs -> Just $ concat
                              [ "The number of characters specified ("
                              , show charCount
                              , ") does not match the number of chararacters found for the following taxa:\n"
                              , unlines $ prettyPrint <$> xs
                              ]                            
        prettyPrint (name, count) = concat ["\t",show name," found (",show count,") characters"]

-- | Consumes everything in the XREAD command prior to the taxa sequences.
-- Produces the expected taxa count and the length of the character sequences.
xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadCharCount <*> xreadTaxaCount)

-- | The superflous information of an XREAD command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceeding the taxa count and character cound parameters
xreadHeader :: MonadParsec s m Char => m ()
xreadHeader =  symbol (string' "xread")
            *> many simpleComment
            *> pure ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''

-- | The number of taxa present in the XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadTaxaCount :: MonadParsec s m Char => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count" 

-- | The number of characters in a taxon sequence for this XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadCharCount :: MonadParsec s m Char => m Int
xreadCharCount = symbol $ flexiblePositiveInt "char count"

-- | Reads one or more taxon sequences.
-- Performs deinterleaving of identically named taxon sequences. 
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT"
-- Right [ ("taxonOne", "GATACA")
--       , ("taxonTwo", "GGAATT")
--       ]
--
-- Interleaved usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT\ntaxonOne ACATAG\ntaxonTwo CCGATC\n"
-- Right [ ("taxonOne", "GATACAACATAG")
--       , ("taxonTwo", "GGAATTCCGATC")
--       ]
xreadSequences :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
xreadSequences = NE.fromList . deinterleaveTaxa <$> symbol (taxonSequence `sepEndBy1` terminal)
  where
    terminal         = whitespaceInline *> endOfLine <* whitespace
    deinterleaveTaxa :: [TaxonInfo] -> [TaxonInfo]
    deinterleaveTaxa = M.toList . fmap DL.toList . foldr f mempty
      where
        f :: TaxonInfo -> Map TaxonName (DList Char) -> Map TaxonName (DList Char)
        f (taxonName, taxonSequence) = insertWith append taxonName (DL.fromList taxonSequence)

-- | Parses an PROCEDURE command that consisits of exacty
-- one of the following:
--
--  * Close file directive
--
--  * Fasta file to read-in
--
--  * Command file to be interpreted
procCommand :: MonadParsec s m Char => m ()
procCommand =  procHeader *> procBody
  where
    procBody = (try procFastaFile   *> pure ())
           <|> (try procCommandFile *> pure ())
           <|>      procCloseFile

-- | Consumes the superflous heading for a PROCEDURE command.
procHeader :: MonadParsec s m Char => m ()
procHeader = string' "proc" *> optional (string' "edure") *> pure ()

-- | A directive to interpret a file. We throw this info away later.
-- Interpreting a file kinda sucks, this ins't Lisp.
procCommandFile :: MonadParsec s m Char => m FilePath
procCommandFile = anythingTill (whitespace *> char ';') <* trim (char ';')

-- | A directive to read in a FASTA file as aligned, non-addative data.
-- Not sure if we should ignore this or acturally process additional files.
procFastaFile :: MonadParsec s m Char => m FilePath
procFastaFile = symbol (char '&') *> procCommandFile

-- | A close file directive. Closes all open files. Found at the end of all
-- properly formated TNT input files.
procCloseFile :: MonadParsec s m Char => m ()
procCloseFile = symbol (char '/') *> symbol (char ';') *> pure ()
    
-- | Parses a taxon name and sequence of characters for a given character.
-- Character values can be one of 64 states ordered @[0..9,A..Z,a..z]@ and also the Chars @\'-\'@ & @\'?\'@.
-- Taxon name cannot contain spaces or the @\';\'@ character.
taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = (,) <$> (symbol taxonName) <*> taxonSeq
  where
    taxonName     = some validNameChar
    taxonSeq      = some validSeqChar
    validNameChar = satisfy (\x -> (not . isSpace) x && x /= ';') -- <* whitespaceInline
    validSeqChar  = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"

-- | Parses an positive integer from a variety of representations.
-- Parses both signed integral values and signed floating values
-- if the value is positive and an integer.
--
-- @flexiblePositiveInt labeling@ uses the @labeling@ parameter to
-- improve ParseError generation.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "1\n"
-- Right 1
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0\n"
-- Left 1:2:
-- expecting rest of number
-- The errorCount value (0) is not a positive number
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "42.0\n"
-- Right 42
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0.1337\n"
-- Left 1:7:
-- expecting 'E', 'e', or rest of number
-- The errorCount value (0.1337) is a real value, not an integral value
-- The errorCount value (0.1337) is not a positive integer
flexiblePositiveInt :: MonadParsec s m Char => String -> m Int
flexiblePositiveInt labeling = either coerceIntegral coerceFloating
                             =<< signed whitespace number <?> ("positive integer for " ++ labeling)
  where
    coerceIntegral :: MonadParsec s m Char => Integer -> m Int
    coerceIntegral x
      | x <= 0      = fail $ concat ["The ",labeling," value (",show x,") is not a positive number"]
      | otherwise   = pure $ fromEnum x
    coerceFloating :: MonadParsec s m Char => Double -> m Int
    coerceFloating x
      | null errors = pure $ fromEnum rounded
      | otherwise   = fails errors
      where
        errors      = catMaybes $ [posError,intError]
        posError    = if x >= 1  then Nothing else Just $ concat ["The ",labeling," value (",show x,") is not a positive integer"]
        intError    = if isInt x then Nothing else Just $ concat ["The ",labeling," value (",show x,") is a real value, not an integral value"]
        isInt n     = n == fromInteger rounded
        rounded     = round x

-- | Consumes trailing whitespace after the parameter combinator.
symbol :: MonadParsec s m Char => m a -> m a
symbol c = c <* whitespace

-- | Consumes trailing whitespace after the parameter combinator.
trim :: MonadParsec s m Char => m a -> m a
trim c = whitespace *> c <* whitespace

-- | Consumes zero or more whitespace characters __including__ line breaks.
whitespace :: MonadParsec s m Char => m ()
whitespace = space

-- | Consumes zero or more whitespace characters that are not line breaks.
whitespaceInline :: MonadParsec s m Char => m ()
whitespaceInline =  inlineSpace

{- ccode crap below.
-- Needs to parse (at least) this crap:

    cc (.) ;
    
    cc - 0.96;
    
    cc - 0.1317;
    
    cc + 82 98 101 104 106 139 147 155 174 179 215 244 245 251 262 265 269 286 289 313 323 346 359 383 386 398 399 435 448 453 479 484 496 497 516 545 546 551 574 591 625 638 639 646 648 661 662 669 687 758 768.773 782 821.825 894 896 897 903 906 1026 1081 1086 1094 1111 1149 1176 1179 1188 1203 1206 1207 1210 1213 1216 1219 1220 1240;
    
    cc - 0.38942;

    cc - 0.81;cc + 82;cc - 83.97;cc + 98;cc - 99.100;cc + 101;cc - 102.103;
    cc + 104;cc - 105;cc + 106;cc - 107.138;cc + 139;cc - 140.146;cc + 147;
    cc - 148.154;cc + 155;cc - 156.173;cc + 174;cc - 175.178;cc + 179;
    cc - 180.214;cc + 215;cc - 216.243;cc + 244.245;cc - 246.250;cc + 251;
    cc - 252.261;cc + 262;cc - 263.264;cc + 265;cc - 266.268;cc + 269;
    cc - 270.285;cc + 286;cc - 287.288;cc + 289;cc - 290.312;cc + 313;
    cc - 314.322;cc + 323;cc - 324.345;cc + 346;cc - 347.358;cc + 359;
    cc - 360.382;cc + 383;cc - 384.385;cc + 386;cc - 387.397;cc + 398.399;
    cc - 400.434;cc + 435;cc - 436.447;cc + 448;cc - 449.452;cc + 453;
    cc - 454.478;cc + 479;cc - 480.483;cc + 484;cc - 485.495;cc + 496.497;
    cc - 498.515;cc + 516;cc - 517.544;cc + 545.546;cc - 547.550;cc + 551;
    cc - 552.573;cc + 574;cc - 575.590;cc + 591;cc - 592.624;cc + 625;
    cc - 626.637;cc + 638.639;cc - 640.645;cc + 646;cc - 647;cc + 648;
    cc - 649.660;cc + 661.662;cc - 663.668;cc + 669;cc - 670.686;cc + 687;
    cc - 688.757;cc + 758;cc - 759.767;cc + 768.773;cc - 774.781;cc + 782;
    cc - 783.820;cc + 821.825;cc - 826.893;cc + 894;cc - 895;cc + 896.897;
    cc - 898.902;cc + 903;cc - 904.905;cc + 906;cc - 907.1025;cc + 1026;
    cc - 1027.1080;cc + 1081;cc - 1082.1085;cc + 1086;cc - 1087.1093;cc + 1094;
    cc - 1095.1110;cc + 1111;cc - 1112.1148;cc + 1149;cc - 1150.1175;cc + 1176;
    cc - 1177.1178;cc + 1179;cc - 1180.1187;cc + 1188;cc - 1189.1202;cc + 1203;
    cc - 1204.1205;cc + 1206.1207;cc - 1208.1209;cc + 1210;cc - 1211.1212;
    cc + 1213;cc - 1214.1215;cc + 1216;cc - 1217.1218;cc + 1219.1220;
    cc - 1221.1239;cc + 1240;cc - 1241.50227;

    ccode /4 0;ccode /4 1;ccode /4 2;ccode /4 3;ccode /4 4;ccode /4 5;ccode /4 6;
    ccode /4 7;ccode /4 8;ccode /4 9;ccode /4 10;ccode /4 11;ccode /4 12;
    ccode /4 13;ccode /4 14;ccode /4 15;ccode /4 16;ccode /4 17;ccode /4 18;
    ccode /4 19;ccode /4 20;ccode /4 21;ccode /4 22;ccode /4 23;ccode /4 24;
    ccode /4 25;ccode /4 26;ccode /4 27;ccode /4 28;ccode /4 29;ccode /4 30;
    ccode /4 31;ccode /4 32;ccode /4 33;ccode /4 34;ccode /4 35;ccode /4 36;
    ccode /4 37;ccode /4 38;ccode /4 39;ccode /4 40;ccode /4 41;ccode /4 42;
    ccode /4 43;ccode /4 44;ccode /4 45;ccode /4 46;ccode /4 47;ccode /4 48;
    ccode /4 49;ccode /4 50;ccode /4 51;ccode /4 52;ccode /4 53;ccode /4 54;
    ccode /4 55;ccode /4 56;ccode /4 57;ccode /4 58;ccode /4 59;ccode /4 60;
    ccode /4 61;ccode /4 62;ccode /4 63;ccode /4 64;ccode /4 65;ccode /4 66;
    ccode /4 67;ccode /4 68;ccode /4 69;ccode /4 70;ccode /4 71;ccode /4 72;
    ccode /4 73;ccode /4 74;ccode /4 75;ccode /4 76;ccode /4 77;ccode /4 78;
-}