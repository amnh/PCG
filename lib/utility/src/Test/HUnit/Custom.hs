-----------------------------------------------------------------------------
-- |
-- Module      :  Test.HUnit.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A module to host custom HUnit functionality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Test.HUnit.Custom
  ( assertException
  ) where

import Control.Exception (ErrorCall, evaluate, try)
import Test.Tasty.HUnit  as HU


-- |
-- Assert that the function should throw an exception on the given input.
assertException
  :: forall a b.
     (a -> b) -- ^ Test function
  ->  a       -- ^ Test input
  -> Assertion
assertException fn a = do
    bErr <- (try $ evaluate (fn a)) :: (IO (Either ErrorCall b))
    case bErr of
      Left  _ -> pure ()
      Right _ -> HU.assertFailure "No exception thrown"
