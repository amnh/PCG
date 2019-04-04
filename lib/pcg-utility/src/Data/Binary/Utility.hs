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

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Utility
  ( getFieldFromBinary
  )
  where
import Control.Lens.Getter    (Getter)
import Control.Lens.Operators ((^.))
import Data.Binary            (Binary, decodeFile)
import System.Directory       (doesFileExist, makeAbsolute)


-- |
-- Deserialized a binary file and applies a getter to the resulting value.
getFieldFromBinary
  :: forall s a . (Binary s)
  => FilePath -> Getter s a -> IO a
getFieldFromBinary filePath getter = do
    fileExist   <- doesFileExist filePath
    absFilePath <- makeAbsolute filePath
    case fileExist of
      False -> error $ "No file found with the specified filepath:\n" <> absFilePath
      True  -> (^. getter) <$> (decodeFile filePath :: IO s)
