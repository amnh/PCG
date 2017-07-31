----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.TRead
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the TREAD command specifying how to read a forests of trees.
-- No validation currently takes place.
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.TNT.Command.TRead where


import           Data.CaseInsensitive
import           Data.Char                (isSpace)
import           Data.Functor             (($>))
import           Data.List                (isSuffixOf)
import qualified Data.List.NonEmpty as NE (fromList)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Parses an TREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
treadCommand :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m TRead
treadCommand = treadValidation =<< treadDefinition
  where
    treadDefinition = symbol treadHeader
                   *> symbol treadForest
                   <* symbol (char ';')

    treadValidation = pure -- No validation yet (what to validate?)


-- |
-- The superflous information of an XREAD command. Consumes the XREAD string
-- identifier and zero or more comments preceeding the taxa count and character
-- cound parameters
treadHeader :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m ()
treadHeader =  symbol (keyword "tread" 2)
            *> many simpleComment
            $> ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''


-- |
-- One or more '*' seperated trees in parenthetical notationy
treadForest :: (MonadParsec e s m, Token s ~ Char) => m TRead
treadForest = fmap NE.fromList $ symbol treadTree `sepBy1` symbol (char '*')


-- |
-- A bifurcating, rooted tree with data only on the leaf nodes.
treadTree :: (MonadParsec e s m, Token s ~ Char) => m TReadTree
treadTree = treadSubtree <|> treadLeaf


-- |
-- A leaf node of the TREAD tree, representing one of the three possible
-- identifier types used for matching with a taxon from the taxa set.
treadLeaf :: (MonadParsec e s m, Token s ~ Char) => m TReadTree
treadLeaf = Leaf <$> choice [try index, try prefix, name] 
 where
   index       = Index  <$>  flexibleNonNegativeInt "taxon reference index"
   prefix      = Prefix <$> (taxaLabel >>= checkTail) 
   name        = Name   <$>  taxaLabel
   taxaLabel   = some labelChar
   labelChar   = satisfy (\x -> not (isSpace x) && x `notElem` "(),;")
   checkTail x = if "..." `isSuffixOf` x then pure x else fail "oops"


-- |
-- A branch of the TREAD tree. each brach can be either a leaf or a sub tree.
treadSubtree :: (MonadParsec e s m, Token s ~ Char) => m TReadTree
treadSubtree = between open close body
  where
    open      = symbol (char '(')
    close     = symbol (char ')')
    body      = Branch <$> some (symbol treadTree)
