------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Save.Evaluate
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Evaluation
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Validation
import Data.FileSource                (FileSource)
import Data.FileSource.IO
import Data.Functor                   (($>))
import Data.Validation
import PCG.Command.Save


-- |
-- Evaluate the 'SaveCommand' by writing out the working graph state in a reloadable format.
evaluate :: SaveCommand -> GraphState -> SearchState
evaluate (SaveCommand fileSource serial) g =
    case serial of
      Binary  -> writeOutBinaryEncoding fileSource g $> g


writeOutBinaryEncoding :: FileSource -> GraphState -> EvaluationT GlobalSettings IO ()
writeOutBinaryEncoding path g = do
    result <- liftIO . runValidationT $ serializeBinary path g
    case result of
      Success _    -> pure ()
      Failure oErr -> failWithPhase Outputting oErr
