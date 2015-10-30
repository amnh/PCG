module PCG.Command.Internal where

import Data.Char             (toLower)
import Data.Either           (partitionEithers)
import Data.Int              (Int64)
import Data.Map              (Map,fromList,lookup)
import Data.Maybe            (fromJust)
import Data.Time.Clock       (DiffTime,secondsToDiffTime)
import Prelude hiding (lookup)

import PCG.Command.Types
import PCG.Script.Types

partitionArguments :: [Argument] -> ([Primative], [Lident], [(Lident,Argument)], [DubiousCommand], [[Argument]])
partitionArguments = foldr f ([],[],[],[],[])
  where
    f (PrimativeArg   x  ) (a,b,c,d,e) = (x:a,   b,       c,   d,   e)
    f (LidentArg      x  ) (a,b,c,d,e) = (  a, x:b,       c,   d,   e)
    f (LidentNamedArg x y) (a,b,c,d,e) = (  a,   b, (x,y):c,   d,   e)
    f (CommandArg     x  ) (a,b,c,d,e) = (  a,   b,       c, x:d,   e)
    f (ArgumentList   x  ) (a,b,c,d,e) = (  a,   b,       c,   d, x:e)
    
partitionPrimatives :: [Primative] -> ([Int64], [Double], [Bool], [String], [DiffTime])
partitionPrimatives = foldr f ([],[],[],[],[])
  where
    f (WholeNum   x) (a,b,c,d,e) = (x:a,   b,   c,   d,   e)
    f (RealNum    x) (a,b,c,d,e) = (  a, x:b,   c,   d,   e)
    f (BitValue   x) (a,b,c,d,e) = (  a,   b, x:c,   d,   e)
    f (TextValue  x) (a,b,c,d,e) = (  a,   b,   c, x:d,   e)
    f (TimeSpan   x) (a,b,c,d,e) = (  a,   b,   c,   d, x:e)
