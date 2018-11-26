{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for collecting 'Left' values of 'Either' forming a 'Validation'
-- similar context.
--
-----------------------------------------------------------------------------

module Data.Binary.Utility
  ( getFieldFromBinary
  )
  where
import Control.Lens.Getter    (Getter)
import Control.Lens.Operators ((^.))
import Data.Binary            (Binary, decodeFile)
import System.Directory       (doesFileExist, makeAbsolute)

getFieldFromBinary
  :: forall s a . (Binary s)
  => FilePath -> Getter s a -> IO a
getFieldFromBinary filePath getter =
  do
    fileExist   <- doesFileExist filePath
    absFilePath <- makeAbsolute filePath
    case fileExist of
      False -> error $
                    "No file found with the specified filepath:\n"
                 <> absFilePath
      True -> do
                (decodeS :: s) <- decodeFile filePath
                pure $ decodeS ^. getter
