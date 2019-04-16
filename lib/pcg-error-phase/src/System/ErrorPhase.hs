------------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation.Unit
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The core monoidal state of an 'Evaluation' monad.
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module System.ErrorPhase
  ( ErrorPhase(..)
  , errorPhaseToExitCode
  ) where

import           Control.DeepSeq
import           Data.Bifunctor            (second)
import           Data.Bimap
import           Data.Bits
import           Data.Data
import           GHC.Generics
import           TextShow
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           System.Exit


-- |
-- Keep track of which phase of the evaluation th error occured in.
--
-- This allows use to use custom exit codes.
data  ErrorPhase
    = Inputing
    | Parsing
    | Unifying
    | Computing
    | Outputing
    deriving (Data, Eq, Generic, Ord, Read, Show)


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
