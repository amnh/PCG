{-# LANGUAGE ScopedTypeVariables #-}
module PCG.Command.Save.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Evaluation
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Validation
import Data.Compact           (getCompact)
import Data.FileSource        (FileSource)
import Data.FileSource.IO
import Data.Functor           (($>))
import Data.Validation
import PCG.Command.Save


evaluate :: SaveCommand -> GraphState -> SearchState
evaluate (SaveCommand fileSource serial) g =
    case serial of
      Binary  -> writeOutBinaryEncoding fileSource g $> g
      Compact -> writeOutCompactRegion  fileSource g $> g
                   

writeOutBinaryEncoding :: FileSource -> GraphState -> EvaluationT (ReaderT GlobalSettings IO) ()
writeOutBinaryEncoding path g = do
    let refDAG = extractReferenceDAG $ getCompact g
    result <- liftIO . runValidationT $ serializeBinary path refDAG
    case result of
      Success _    -> pure ()
      Failure oErr -> state $ failWithPhase Outputing oErr


writeOutCompactRegion :: FileSource -> GraphState -> EvaluationT (ReaderT GlobalSettings IO) ()
writeOutCompactRegion path g = do
    result <- liftIO . runValidationT $ serializeCompact path g
    case result of
      Success _    -> pure ()
      Failure oErr -> state $ failWithPhase Outputing oErr
