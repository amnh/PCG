-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation.Notification
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The various 'Notification' values in an 'Contol.Evaluation.Evaluation' monad.
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Evaluation.Notification
  ( Notification(..)
  , renderNotification
  ) where

import Control.DeepSeq
import Data.Data
import Data.Text.Lazy            (Text)
import GHC.Generics              hiding (Prefix)
import Test.QuickCheck
import Test.QuickCheck.Instances ()


-- |
-- Represents the types of contextual notifications that can be generated by
-- the evaluation.
data  Notification
    = Warning     Text
    | Information Text
    deriving stock    (Data, Eq, Generic, Ord, Show, Typeable)
    deriving anyclass (NFData)


instance Arbitrary Notification where

    {-# INLINE arbitrary #-}

    arbitrary = oneof [ Information <$> arbitrary, Warning <$> arbitrary ]


instance CoArbitrary Notification where

    {-# INLINE coarbitrary #-}

    coarbitrary = genericCoarbitrary


renderNotification :: Notification -> Text
renderNotification (Warning     x) = x
renderNotification (Information x) = x
