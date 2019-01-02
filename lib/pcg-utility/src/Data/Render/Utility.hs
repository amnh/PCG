{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Render.Utility
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for rendering text and workinng with the TextShow typeclass
--
-----------------------------------------------------------------------------


module Data.Render.Utility
  ( writeFileT
  ) where

import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text              (Text)
import Data.Text.IO           (hPutStr)
import Pipes                  (Producer, for, runEffect, yield)
import Prelude                hiding (writeFile)
import System.IO              (IOMode, hClose, openFile)
import TextShow


-- |
-- Streams text to a file in constant memory for any type with a `TextShow`
-- instance.
writeFileT :: TextShow text => IOMode -> FilePath -> text -> IO ()
writeFileT mode fp text = do
  h <- openFile fp mode
  let builder = showb text
  runEffect $ for (builderTextProducer builder) (liftIO . hPutStr h)
  hClose h

-- |
-- Efficiently yield text from a 'Builder' type.
builderTextProducer :: Monad m => Builder -> Producer Text m ()
builderTextProducer = yield . toText
