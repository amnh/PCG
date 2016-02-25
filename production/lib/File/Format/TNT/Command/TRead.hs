{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.TRead where

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntSet              (IntSet, singleton)
import qualified Data.IntSet        as IS (fromList)
import           Data.List                (isSuffixOf,intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an TREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
treadCommand :: MonadParsec s m Char => m TRead
treadCommand = treadValidation =<< treadDefinition
  where
    treadDefinition :: MonadParsec s m Char => m TRead
    treadDefinition = symbol treadHeader
                   *> symbol treadForest
                   <* symbol (char ';')

    treadValidation :: MonadParsec s m Char => TRead -> m TRead
    treadValidation = pure

-- | The superflous information of an XREAD command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceeding the taxa count and character cound parameters
treadHeader :: MonadParsec s m Char => m ()
treadHeader =  symbol (keyword "tread" 2)
            *> many simpleComment
            *> pure ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''

-- | One or more '*' seperated trees in parenthetical notationy
treadForest :: MonadParsec s m Char => m TRead
treadForest = fmap NE.fromList $ symbol treadTree `sepBy1` symbol (char '*')

treadTree :: MonadParsec s m Char => m TReadTree
treadTree = treadSubtree <|> treadLeaf

treadLeaf :: MonadParsec s m Char => m TReadTree
treadLeaf = Leaf <$> choice [try index, try prefix, name] 
 where
   index       = Index  <$>  flexibleNonNegativeInt "taxon reference index"
   prefix      = Prefix <$> (taxaLabel >>= checkTail) 
   name        = Name   <$>  taxaLabel
   taxaLabel   = some labelChar
   labelChar   = satisfy (\x -> not (isSpace x) && x `notElem` "(),;")
   checkTail x = if "..." `isSuffixOf` x then pure x else fail "oops"

treadSubtree :: MonadParsec s m Char => m TReadTree
treadSubtree = between open close body
  where
    open      = symbol (char '(')
    close     = symbol (char ')')
    body      = Branch <$> some (symbol treadTree)
