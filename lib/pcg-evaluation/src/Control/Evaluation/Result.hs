------------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation.Result
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The core semigroupoid state of an 'Control.Evaluation.Evaluation' monad.
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.Evaluation.Result where

import Control.DeepSeq
import Control.Monad.Fix         (MonadFix)
import Control.Monad.Zip         (MonadZip (..))
import Data.Data
import Data.Functor.Alt          (Alt (..))
import Data.Functor.Apply        (Apply (..))
import Data.Functor.Bind         (Bind (..))
import Data.Functor.Classes      (Eq1, Ord1 (..), Show1)
import Data.Semigroup            (Semigroup (..))
import Data.Text.Lazy            (Text, pack)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TextShow


-- |
-- The internal state of the computation. A short-circuiting evaluation unit
-- which returns either an error that occured preventing the evaluation for being
-- completed or a value of the evaluation or
--
-- In the case that an error occured, an 'ErrorPhase' is stored along with a
-- 'Text' value describing the error.
--
-- Note that multiple errors can be aggregated before calling 'fail' or
-- 'evalUnitWithPhase' using another 'Applicative' or 'Monad' locally. We will
-- use the @Validation@ type to collect many error of the same "phase" before
-- failing in the 'Control.Evaluation' monad. Consequently, the textual error message can
-- be quite long, representing the entire list of aggregated failures. We use
-- 'Text' instead of 'String' to store the error message to save space and
-- efficient rendering.
newtype EvaluationResult a = EU { runEvaluationResult :: Either (ErrorPhase, Text) a }
   deriving stock    (Data, Eq, Foldable, Generic, Generic1, Show, Traversable, Typeable)
   deriving anyclass (NFData)
   deriving newtype  (Applicative, Apply, Eq1, Functor, MonadFix, Show1)

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


instance Alt EvaluationResult where

    {-# INLINEABLE (<!>) #-}

    lhs <!> rhs =
        case runEvaluationResult lhs of
          Right _ -> lhs
          _       -> rhs


instance Arbitrary ErrorPhase where

    {-# INLINE arbitrary #-}

    arbitrary = elements [ Inputing, Parsing, Unifying, Computing, Outputing ]


instance Arbitrary a => Arbitrary (EvaluationResult a) where

    {-# INLINE arbitrary #-}

    arbitrary = liftArbitrary arbitrary


instance Arbitrary1 EvaluationResult where

    {-# INLINE liftArbitrary #-}

    liftArbitrary g = do
        n <- choose (0, 9) :: Gen Word -- 1/10 chance of 'error' value
        case n of
          0 -> (`evalUnitWithPhase` errorMessage) <$> arbitrary
          _ -> pure <$> g
      where
        errorMessage :: Text
        errorMessage = "Error Description"


instance CoArbitrary a => CoArbitrary (EvaluationResult a) where

    {-# INLINE coarbitrary #-}

    coarbitrary = genericCoarbitrary


instance CoArbitrary ErrorPhase where

    {-# INLINE coarbitrary #-}

    coarbitrary = genericCoarbitrary


instance Bind EvaluationResult where

    {-# INLINEABLE (>>-) #-}
    {-# INLINE     join  #-}

    e >>- f =
        case runEvaluationResult e of
          Left  l -> EU . Left $ l
          Right v -> f v

    join e =
        case runEvaluationResult e of
          Left  l -> EU . Left $ l
          Right v -> v


instance Monad EvaluationResult where

    {-# INLINEABLE (>>=)  #-}
    {-# INLINE     (>>)   #-}
    {-# INLINE     return #-}

    (>>=)  = (>>-)

    (>>)   = (*>)

    return = pure


instance MonadFail EvaluationResult where

    {-# INLINE fail #-}

    fail = EU . Left . (\x -> (Computing, x)) .
        \case
           []   -> "Unspecified error."
           x:xs -> pack $ x:xs


instance MonadZip EvaluationResult where

    {-# INLINEABLE mzip     #-}
    {-# INLINEABLE munzip   #-}
    {-# INLINE     mzipWith #-}

    mzip     = liftF2 (,)

    mzipWith = liftF2

    munzip x =
        case runEvaluationResult x of
          Left  s     -> (EU $ Left s, EU $ Left s)
          Right (a,b) -> (pure a, pure b)


instance NFData ErrorPhase where

    {-# INLINE rnf #-}

    rnf x = x `seq` ()


instance Ord a => Ord (EvaluationResult a) where

    {-# INLINE compare #-}

    compare  = liftCompare compare


instance Ord1 EvaluationResult where

    {-# INLINE liftCompare #-}

    liftCompare cmp lhs rhs =
        case (runEvaluationResult lhs, runEvaluationResult rhs) of
          (Left  x, Left  y) -> x `compare` y
          (Left  _, Right _) -> GT
          (Right _, Left  _) -> LT
          (Right x, Right y) -> x `cmp` y


instance Semigroup (EvaluationResult a) where

   lhs <> rhs =
       case lhs of
         EU (Left _) -> lhs
         _           -> rhs

   stimes _ e = e


-- |
-- Create a failure result with a specified 'ErrorPhase'.
--
-- Use in place of 'fail' when you want to the associated 'ErrorPhase' to be a value other than 'Computing'.
{-# INLINE[1] evalUnitWithPhase #-}
evalUnitWithPhase :: TextShow s => ErrorPhase -> s -> EvaluationResult a
evalUnitWithPhase p s = EU $ Left (p, showtl s)
{-# RULES
"evalUnitWithPhase/Text" forall p (s :: Text). evalUnitWithPhase p s = EU $ Left (p, s)
  #-}

