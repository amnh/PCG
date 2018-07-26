{-# LANGUAGE ScopedTypeVariables #-}

module Test.HUnit.Custom
  (assertException
  ) where

import Test.Tasty.HUnit as HU
import Control.Exception (try, ErrorCall, evaluate)

assertException :: forall a b.
                   (a -> b) -> -- test function
                   a        -> -- test input
                   Assertion
assertException fn a = do
    bErr <- (try $ evaluate (fn a)) :: (IO (Either ErrorCall b))
    case bErr of
      Left  _ -> return ()
      Right _ -> HU.assertFailure "No exception thrown"
