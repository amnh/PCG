{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.CCode where

{-- TODO:
  - Robust tests
  - Good documentation
  -}

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntSet              (IntSet, singleton)
import qualified Data.IntSet        as IS (fromList)
import           Data.List                (intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

data CharacterState
     = Additive
     | NonAdditive
     | Active
     | NonActive
     | Sankoff
     | NonSankoff
     | Weight Int
     | Steps  Int
     deriving (Show)

data CharacterSet
   = Single    Int
   | Range     Int Int
   | FromStart Int
   | ToEnd     Int
   | Whole
   deriving (Show)

data CharacterChange = Change CharacterState (NonEmpty CharacterSet) deriving (Show)

data CharacterMetaData
   = CharMeta
   { aligned :: Bool
   , active  :: Bool
   , sankoff :: Bool
   , weight  :: Int
   , steps   :: Int
   } deriving (Show)

-- | Parses a CCODE command that consists of:
--
--  * A single specification of the character state change
--
--  * One or more character indicies or index ranges of affected characters
ccodeCommand :: MonadParsec s m Char => m CharacterChange
ccodeCommand = ccodeHeader *> ccodeBody
  where
    ccodeBody = ccodeAdditive
            <|> ccodeNonAdditive
            <|> ccodeActive
            <|> ccodeNonActive
            <|> ccodeSankoff
            <|> ccodeNonSankoff
            <|> ccodeWeight
            <|> ccodeSteps

-- | Consumes the superflous heading for a CCODE command.
ccodeHeader :: MonadParsec s m Char => m ()
ccodeHeader = keyword "ccode" 2

-- | Parses a single character index or a contiguous character range
ccodeIndicies :: MonadParsec s m Char => m CharacterSet
ccodeIndicies = choice $ try <$> [range, fromStart, single, toEnd, whole]
  where
    range     = Range     <$> num <* dot <*> num
    fromStart = FromStart <$> num <* dot
    single    = Single    <$> num
    toEnd     = dot *> (ToEnd <$> num)
    whole     = dot *> pure Whole
    num       = symbol nonNegInt
    dot       = symbol (char '.')

-- | A Uitility function for creating 'CharacterChange' combinators
ccodeMetaChange :: MonadParsec s m Char => Char -> CharacterState -> m CharacterChange
ccodeMetaChange c s = do
    _ <- symbol (char c)
    i <- NE.fromList <$> some ccodeIndicies
    pure $ Change s i

-- | Parses a _Additive_ specification `CharacterChange`
ccodeAdditive    :: MonadParsec s m Char => m CharacterChange
ccodeAdditive    = ccodeMetaChange '+' Additive

-- | Parses a _Non-Additive_ specification `CharacterChange`
ccodeNonAdditive :: MonadParsec s m Char => m CharacterChange
ccodeNonAdditive = ccodeMetaChange '-' NonAdditive

-- | Parses a _Active_ specification `CharacterChange`
ccodeActive      :: MonadParsec s m Char => m CharacterChange
ccodeActive      = ccodeMetaChange '[' Active

-- | Parses a _Non-Active_ specification `CharacterChange`
ccodeNonActive   :: MonadParsec s m Char => m CharacterChange
ccodeNonActive   = ccodeMetaChange ']' NonActive

-- | Parses a _Sankoff_ specification `CharacterChange`
ccodeSankoff     :: MonadParsec s m Char => m CharacterChange
ccodeSankoff     = ccodeMetaChange '(' Sankoff

-- | Parses a _Non-Sankoff_ specification `CharacterChange`
ccodeNonSankoff :: MonadParsec s m Char => m CharacterChange
ccodeNonSankoff  = ccodeMetaChange ')' NonSankoff

-- | Parses a _weight_ specification `CharacterChange`
ccodeWeight :: MonadParsec s m Char => m CharacterChange
ccodeWeight = do
    _ <- symbol (char '/')
    w <- Weight <$> (symbol nonNegInt)
    i <- NE.fromList <$> some ccodeIndicies
    pure $ Change w i

-- | Parses a _step_ specification `CharacterChange`
ccodeSteps :: MonadParsec s m Char => m CharacterChange
ccodeSteps = do
    _ <- symbol (char '=')
    w <- Steps <$> (symbol nonNegInt)
    i <- NE.fromList <$> some ccodeIndicies
    pure $ Change w i

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
