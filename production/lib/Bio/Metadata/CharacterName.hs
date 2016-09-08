------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.CharacterName
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data types for metadata
--
-----------------------------------------------------------------------------

module Bio.Metadata.CharacterName
  ( CharacterName(..)
  ) where

data CharacterName
   = UserDefined FilePath String
   | Default     FilePath Int
   deriving (Eq)

sourceFile :: CharacterName -> FilePath
sourceFile (UserDefined x _) = x
sourceFile (Default     x _) = x

makeUserDefinedCharacterName :: FilePath -> String -> CharacterName
makeUserDefinedCharacterName = UserDefined

makeDefaultCharacterName :: FilePath -> Int -> CharacterName
makeDefaultCharacterName = Default

instance Show CharacterName where
  show (UserDefined _ name) = name
  show (Default path index) = path <> ":" <> show index

instance Ord CharacterName where
  lhs@Default{} `compare` rhs@UserDefined{} =
    case rhs `compare` lhs of
      LT -> GT
      GT -> LT
      EQ -> EQ
  lhs@(UserDefined _ name) `compare` rhs@(Default path index)
    | path <> ":" `isPrefixOf` name = LT
    | otherwise = strCmp lhs rhs
  compare = strCmp

strCmp lhs rhs = show lhs `compare` show rhs
