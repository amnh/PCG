------------------------------------------------------------------------------
-- |
-- Module      :  System.ErrorPhase
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Defines the different phases in which an error can occur.
--
-- Each 'ErrorPhase' can be uniquely translated to an 'ExitCode'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module System.ErrorPhase
  ( ErrorPhase(..)
  , errorPhaseToExitCode
  , exitCodeToInt
  ) where

import Control.DeepSeq
import Data.Bifunctor            (second)
import Data.Bimap
import Data.Bits
import Data.Data
import GHC.Generics
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TextShow


-- |
-- Define which phase of a computation the error occured in.
--
-- Combining this with another "error" monand allows the use of custom exit codes.
data  ErrorPhase
    = Inputing
    | Parsing
    | Unifying
    | Computing
    | Outputing
    deriving stock (Data, Eq, Generic, Ord, Read, Show)


instance Arbitrary ErrorPhase where

    {-# INLINE arbitrary #-}

    arbitrary = elements [ Inputing, Parsing, Unifying, Computing, Outputing ]


instance CoArbitrary ErrorPhase where

    {-# INLINE coarbitrary #-}

    coarbitrary = genericCoarbitrary


instance NFData ErrorPhase where

    rnf x = x `seq` ()


instance TextShow ErrorPhase where

    showb = fromText .
        \case
           Inputing  -> "Inputing"
           Parsing   -> "Parsing"
           Unifying  -> "Unifying"
           Computing -> "Computing"
           Outputing -> "Outputing"


-- |
-- Definition of the unique mapping between 'ErrorPhase' and 'ExitCode'.
--
-- The 'ExitCode' returned from this function will return an 8 bit value.
-- 'ExitCode' exposes a 'Int', and not a 'Data.Word.Word8' or 'Data.Int.Int8' as described, however
-- we limit the range of the resulting 'ExitCode'.
--
-- The resulting 8-bit exit code value will have the two least significant bits
-- cleared and the two most significant bits cleared. These are reserved for
-- future use.
--
-- In the case of an input error, the third least significant bit will be set:
--
-- * @00000100 = 4@
--
-- In the case of a parse error, the fourth least significant bit will be set:
--
-- * @00001000 = 8@
--
-- In the case of a unification error, the third and fourth least significant bit
-- will be set:
--
-- * @00001100 = 12@
--
-- In the case of a computation error, the fifth least significant bit will be set:
--
-- * @00010000 = 16@
--
-- In the case of an output error, the sixth least significant bit will be set:
--
-- * @00100000 = 32@
errorPhaseToExitCode :: Bimap ErrorPhase ExitCode
errorPhaseToExitCode = fromAscPairList . force $ second buildExitCode <$>
    [ ( Inputing, [2]  )
    , (  Parsing, [3]  )
    , ( Unifying, [2,3])
    , (Computing, [4]  )
    , (Outputing, [5]  )
    ]
  where
    buildExitCode = ExitFailure . foldr ((.|.) . bit) zeroBits


-- |
-- Extract the 'Int' value from an 'ExitCode'.
--
-- Assumes that the value 'ExitSuccess' has an equivelent numeric value of @0@.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt  ExitSuccess    = 0
exitCodeToInt (ExitFailure i) = i
