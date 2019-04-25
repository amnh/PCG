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
-- The core semigroupoid state of an 'Evaluation' monad.
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

module Control.Evaluation.Unit where

import           Control.DeepSeq
import           Control.Monad.Fail        (MonadFail)
import qualified Control.Monad.Fail        as F
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Zip         (MonadZip (..))
import           Data.Data
import           Data.Functor.Alt          (Alt (..))
import           Data.Functor.Apply        (Apply (..))
import           Data.Functor.Bind         (Bind (..))
import           Data.Functor.Classes      (Eq1, Ord1 (..), Show1)
import           Data.Text.Lazy            (Text, pack)
import           GHC.Generics
import           System.ErrorPhase
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           TextShow


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
-- failing in the 'Evaluation' monad. Consequently, the textual error message can
-- be quite long, representing the entire list of aggregated failures. We use
-- 'Text' instead of 'String' to store the error message to save space and
-- efficient rendering.
newtype EvalUnit a = EU { runEvalUnit :: Either (ErrorPhase, Text) a }
   deriving ( Applicative
            , Apply
            , Data
            , Eq
            , Eq1
            , Foldable
            , Functor
            , Generic
            , Generic1
            , MonadFix
            , NFData
            , Semigroup
            , Show
            , Show1
            , Traversable
            )

instance Alt EvalUnit where

    {-# INLINEABLE (<!>) #-}

    lhs <!> rhs =
        case runEvalUnit lhs of
          Right _ -> lhs
          _       -> rhs


instance Arbitrary a => Arbitrary (EvalUnit a) where

    {-# INLINE arbitrary #-}

    arbitrary = liftArbitrary arbitrary


instance Arbitrary1 EvalUnit where

    {-# INLINE liftArbitrary #-}

    liftArbitrary g = do
        n <- choose (0, 9) :: Gen Word -- 1/10 chance of 'error' value
        case n of
          0 -> (`evalUnitWithPhase` errorMessage) <$> arbitrary
          _ -> pure <$> g
      where
        errorMessage :: Text
        errorMessage = "Error Description"


instance CoArbitrary a => CoArbitrary (EvalUnit a) where

    {-# INLINE coarbitrary #-}

    coarbitrary = genericCoarbitrary


instance Bind EvalUnit where

    {-# INLINEABLE (>>-)  #-}
    {-# INLINE   join  #-}

    e >>- f =
        case runEvalUnit e of
          Left  l -> EU . Left $ l
          Right v -> f v

    join e =
        case runEvalUnit e of
          Left  l -> EU . Left $ l
          Right v -> v


instance Monad EvalUnit where

    {-# INLINEABLE (>>=)  #-}
    {-# INLINE     (>>)   #-}
    {-# INLINE     return #-}
    {-# INLINE     fail   #-}

    (>>=)  = (>>-)

    (>>)   = (*>)

    return = pure

    fail   = F.fail


instance MonadFail EvalUnit where

    {-# INLINE fail #-}

    fail = EU . Left . (\x -> (Computing, x)) .
        \case
           []   -> "Unspecified error."
           x:xs -> pack $ x:xs


instance MonadZip EvalUnit where

    {-# INLINEABLE mzip     #-}
    {-# INLINEABLE munzip   #-}
    {-# INLINE     mzipWith #-}

    mzip     = liftF2 (,)

    mzipWith = liftF2

    munzip x =
        case runEvalUnit x of
          Left  s     -> (EU $ Left s, EU $ Left s)
          Right (a,b) -> (pure a, pure b)


instance Ord a => Ord (EvalUnit a) where

    {-# INLINE compare #-}

    compare  = liftCompare compare


instance Ord1 EvalUnit where

    {-# INLINE liftCompare #-}

    liftCompare cmp lhs rhs =
        case (runEvalUnit lhs, runEvalUnit rhs) of
          (Left  x, Left  y) -> x `compare` y
          (Left  _, Right _) -> GT
          (Right _, Left  _) -> LT
          (Right x, Right y) -> x `cmp` y


{-# INLINE[1] evalUnitWithPhase #-}
evalUnitWithPhase :: TextShow s => ErrorPhase -> s -> EvalUnit a
evalUnitWithPhase p s = EU $ Left (p, showtl s)
{-# RULES
"evalUnitWithPhase/Text" forall p (s :: Text). evalUnitWithPhase p s = EU $ Left (p, s)
  #-}

