{-# LANGUAGE ApplicativeDo #-}
module PCG.Command.Save.Evaluate
  ( evaluate
  , saveFile
  )
  where

import           Data.Compact.Serialize                    (writeCompact)
import           Bio.Graph
import           Control.Monad.IO.Class (liftIO)

evaluate :: GraphState -> SearchState
evaluate g = do
  liftIO $ writeCompact saveFile g
  pure g


saveFile :: FilePath
saveFile = ".pcg.save"
