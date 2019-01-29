{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Monad.Logger.Test
  ( testSuite
  ) where

import Control.Evaluation    (Evaluation)
import Control.Monad.Logger
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "Logger Typeclass Laws"
    [ loggerLaws
    ]

{- |

  A 'MonadFail' that has been extended to support "information" and "warning"
  level messages.

  Typeclass Laws:

  Failure nullification:

 > fail x >> info y === fail x
 > fail x >> warn y === fail x

  Assocativity:

 > info x >> (info y >> info z) === (info x >> info y) >> info z
 > warn x >> (warn y >> warn z) === (warn x >> warn y) >> warn z

-}

loggerLaws :: TestTree
loggerLaws = testGroup "Laws" [failureNullificationLaws, associativityLaws]
  where
    failureNullificationLaws = testGroup "Failure Nullification Laws" [infoFail, warnFail]
    associativityLaws        = testGroup "Assocaiativity Laws"        [infoAssoc, warnAssoc]


    infoFail = testGroup "Info nullification"
                 [testProperty
                  "fail x >> info y === fail x"
                  (failureInfo @Evaluation @Int)
                 ]
    warnFail = testGroup "Warn nullification"
                 [testProperty
                  "fail x >> warn y === fail x"
                  (failureWarn @Evaluation @Int)
                 ]


    infoAssoc = testGroup "Info associativity:"
                  [testProperty
                   "info x >> (info y >> info z) === (info x >> info y) >> info z"
                   (assocInfo @Evaluation @Int)
                  ]

    warnAssoc = testGroup "Warn associativity"
                  [testProperty
                   "warn x >> (warn y >> warn z) === (warn x >> warn y) >> warn z"
                   (assocWarn @Evaluation @Int)
                  ]


failureInfo :: forall m a . (Logger m a, Eq (m a)) => String -> String -> Bool
failureInfo x y =
  (==) @(m a) (fail x >> info y) (fail x)


failureWarn :: forall m a . (Logger m a, Eq (m a)) => String -> String -> Bool
failureWarn x y =
  (==) @(m a) (fail x >> warn y) (fail x)


assocInfo
  :: forall m a . (Logger m a, Eq (m a))
  => String
  -> String
  -> String
  -> Bool
assocInfo x y z =
  (info @m @a x >> (info @m @a y >> info @m @a z)) ==
  ((info @m @a x >> info @m @a y) >> info @m @a z)


assocWarn
  :: forall m a . (Logger m a, Eq (m a))
  => String
  -> String
  -> String
  -> Bool
assocWarn x y z =
  (warn @m @a x >> (warn @m @a y >> warn @m @a z)) ==
  ((warn @m @a x >> warn @m @a y) >> warn @m @a z)

