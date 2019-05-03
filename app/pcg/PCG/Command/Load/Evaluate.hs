{-# LANGUAGE ScopedTypeVariables #-}
module PCG.Command.Load.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Compact.Serialize (unsafeReadCompact)
import Data.MonoTraversable
import PCG.Command.Load


evaluate :: LoadCommand -> SearchState
evaluate (LoadCommand filePath) = do
    (optGraphState :: Either String GraphState) <- liftIO . unsafeReadCompact $ otoList filePath
    case optGraphState of
      Left  err        ->  fail $ "Failed to read savefile with error: \n" <> err
      Right graphState ->  pure graphState :: SearchState
