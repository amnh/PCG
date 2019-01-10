{-# LANGUAGE ScopedTypeVariables #-}
module PCG.Command.Load.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Compact.Serialize (unsafeReadCompact)
import PCG.Command.Load


evaluate :: LoadCommand -> a -> SearchState
evaluate (LoadCommand filePath) _ = do
  (optGraphState :: Either String GraphState) <- liftIO $ unsafeReadCompact filePath
  case optGraphState of
    Left  err        ->  fail $ "Failed to read savefile with error: \n" <> err
    Right graphState ->  pure graphState :: SearchState
