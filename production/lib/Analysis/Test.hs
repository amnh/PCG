module Analysis.Test where

import Analysis.GenericFitch

testSuite :: TestTree
testSuite = testGroup "Tree Analysis Tests" [fitchTests]

fitchTests :: TestTree
fitchTests = testGroup "Test Fitch algorithm" []

-- verify cost no more than length of sequence