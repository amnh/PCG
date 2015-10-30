module PCG.Command.Types.Read
  ( validate
  ) where

import Data.Char       (toLower)
import Data.Either     (partitionEithers)
import Data.Int        (Int64)
import Data.Map        (Map,fromList,lookup)
import Data.Maybe      (fromJust)
import Data.Time.Clock (DiffTime,secondsToDiffTime)
import Prelude  hiding (lookup)

import PCG.Command.Internal
import PCG.Command.Types
import PCG.Script.Types

validate :: [Argument] -> Either String Command
validate xs
  | noArgs        = Left "No arguments provided to the 'read' command! The 'read' command expects one or more arguments"
  | notAllStrings = Left "One or more arguments provided to the 'read' command are not strings! The 'read' command expects one or more string arguments."
  | otherwise     = Right $ READ s
  where
    noArgs        = null xs
    x@(p,l,n,c,a) = partitionArguments xs
    y@(i,r,b,s,t) = partitionPrimatives p
    notAllStrings = case (x,y) of
                      ((_,[],[],[],[]),([],[],[],_,[])) -> False
                      _                                 -> True
