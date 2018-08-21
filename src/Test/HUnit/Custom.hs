{-# LANGUAGE ScopedTypeVariables #-}

module Test.HUnit.Custom
  (assertException
  ) where

import Control.Exception (ErrorCall, evaluate, try)
import Test.Tasty.HUnit  as HU

assertException :: forall a b.
                   (a -> b) -> -- test function
                   a        -> -- test input
                   Assertion
assertException fn a = do
    bErr <- (try $ evaluate (fn a)) :: (IO (Either ErrorCall b))
    case bErr of
      Left  _ -> pure ()
      Right _ -> HU.assertFailure "No exception thrown"
