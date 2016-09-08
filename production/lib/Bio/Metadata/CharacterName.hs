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
  ( CharacterName()
  , isUserDefined
  , makeCharacterNames
  , sourceFile
  ) where

import Data.List   (isPrefixOf)
import Data.Monoid
import Text.Show   (showListWith, showString)

data CharacterName
   = UserDefined FilePath String
   | Default     FilePath Int
   deriving (Eq)

-- A custom 'Show' instance for more legible rendering of lists
instance Show CharacterName where
  show (UserDefined _ name) = name
  show (Default path index) = path <> ":" <> show index

  showList = showListWith f
    where
      f x = showString $ "\"" <> show x <> "\""

-- Ordering biases user defined names with a file path prefix before defaulted names with the same prefix.
instance Ord CharacterName where
  lhs@Default{} `compare` rhs@UserDefined{} =
    case rhs `compare` lhs of
      LT -> GT
      GT -> LT
      EQ -> EQ
  lhs@(UserDefined _ name) `compare` rhs@(Default path index)
    | (path <> ":") `isPrefixOf` name = LT
    | otherwise = strCmp lhs rhs
  compare lhs rhs = strCmp lhs rhs

-- Used internally for orderign logic after special cases are checked.
strCmp :: Show a => a -> a -> Ordering
strCmp lhs rhs = show lhs `compare` show rhs



-- | Determine if the CharacterName was user defined or defaulted.
isUserDefined :: CharacterName -> Bool
isUserDefined UserDefined{} = True
isUserDefined _             = False


-- | Determine the source file for the character.
sourceFile :: CharacterName -> FilePath
sourceFile (UserDefined x _) = x
sourceFile (Default     x _) = x

-- | Construct many 'CharacterName's for the input list of '(FilePath, Maybe String)' pairs.
--   'Just' values represent user supplied character names while 'Nothing' values represent
--   character names to be defaulted. However, in some cases 'Just' valued strings may be
--   rejected and defaulted instead. This occurs when the user defined character name starts
--   with a ":" or is empty.
--
--   The construction is order preserving on the input structure. Multiple characters from a
--   ffrom the same input file will be defualted with incrementing indices.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> makeCharacterNames [("file.dat", Nothing)]
-- ["file.dat:0"]
--
-- >>> makeCharacterNames [("file.dat", Just "Eyes")]
-- ["Eyes"]
--
-- >>> makeCharacterNames [("path/to/file.dat", Nothing")]
-- ["file.dat:0"]
--
-- >>> makeCharacterNames [("path/to/file.dat", Just "Eyes")]
-- ["Eyes"]
--
-- >>> makeCharacterNames [("file.dat", Nothing), ("file.dat", Nothing)]
-- ["file.dat:0","file.dat:1"]
--
-- >>> makeCharacterNames [("file.dat", Nothing), ("file.dat", Just "Eyes"), ("file.dat", Nothing)]
-- ["file.dat:0","Eyes","file.dat:1"]
--
-- >>> makeCharacterNames [("foo.txt", Nothing), ("foo.tx", Nothing), ("bar.txt", Nothing"), ("baz.txt",Nothing), ("baz.txt",Nothing)]
-- ["foo.txt:0","foo.txt:1","bar.txt:0","baz.txt:0","baz.txt"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just ":")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just ":a")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "a:")
-- ["a:"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "%#:!")
-- ["%#:!"]
--
-- >>> makeCharacterNames [("foo.txt", Nothing), ("foo.tx", Just ""), ("foo.tx", Nothing)]
-- ["foo.txt:0","foo.txt:1","foo.txt:2"]
-- 
makeCharacterNames :: (Foldable f, Functor f,) => f (FilePath, Maybe String) => f CharacterName
makeCharacterNames xs = undefined

{-
-- | Constructor for a 'CharacterName' that has been specified explicitly by user input.
makeUserDefinedCharacterName :: FilePath -> String -> CharacterName
makeUserDefinedCharacterName path name
  | null path = makeDefaultCharacterName path 
  | null name = 
  | = UserDefined

-- | Constructor for a 'CharacterName' that has needs to be defaulted.
-- Please don't pass in an 
makeDefaultCharacterName :: FilePath -> Int -> CharacterName
makeDefaultCharacterName = Default

-- | Construct many sequentially indexed default 'CharacterName's for the suppleid range and 'FilePath'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> makeDefaultCharacterNameRange "path/to/file.dat" 3 5
-- ["file.dat:3","file.dat:4","file.dat:5"]
--
-- >>> makeDefaultCharacterNameRange "foo.bar" 1 1
-- ["foo.bar:1"]
--
-- >>> makeDefaultCharacterNameRange "virus.exe" (-6) 2
-- ["virus.exe:0","virus.exe:1",virus.exe:2"]
--
-- >>> makeDefaultCharacterNameRange "neg.val" (-3) (-1)
-- []
--
-- >>> makeDefaultCharacterNameRange "high.low" 8 6
-- []
-- 
makeDefaultCharacterNameRange :: FilePath -> Int -> Int -> [CharacterName]
makeDefaultCharacterNameRange path lower upper
  | upper < 0 = []
  | otherwise = Default path <$> [max lower 0 .. upper]
-}
