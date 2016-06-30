{-# LANGUAGE FlexibleContexts #-}

module Test.Custom (module X) where

import Test.Custom.Constructions as X
import Test.Custom.Parse         as X
import Test.Custom.Tree          as X
import Test.Custom.Types ()
import Test.QuickCheck.Arbitrary.Instances ()
