{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick.Parser
  ( NewickNode()
  , newickStandardDefinition
  , newickExtendedDefinition
  , newickForestDefinition
  ) where

import Control.Monad              (liftM)
import Data.Map            hiding (filter,foldl,foldr,null)
import Data.Maybe                 (fromJust,fromMaybe,isJust)
import File.Format.Newick.Internal
import Prelude             hiding (lookup)
import Text.Parsec         hiding (label)
import Text.Parsec.Custom

newickStandardDefinition :: Stream s m Char => ParsecT s u m NewickNode
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')

newickExtendedDefinition :: Stream s m Char => ParsecT s u m NewickNode
newickExtendedDefinition = newickStandardDefinition >>= joinNonUniqueLabeledNodes

newickForestDefinition :: Stream s m Char => ParsecT s u m NewickForest
newickForestDefinition = whitespace *> symbol (char '<') *> many newickExtendedDefinition <* symbol (char '>')

newickNodeDefinition :: Stream s m Char => ParsecT s u m NewickNode
newickNodeDefinition = do
    descendants'  <- descendantListDefinition
    label'        <- optionMaybe newickLabelDefinition
    branchLength' <- optionMaybe branchLengthDefinition
    pure $ NewickNode descendants' label' branchLength'

descendantListDefinition :: Stream s m Char => ParsecT s u m [NewickNode]
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace

subtreeDefinition :: Stream s m Char => ParsecT s u m NewickNode
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition

newickLeafDefinition :: Stream s m Char => ParsecT s u m NewickNode
newickLeafDefinition = do
    label'        <- newickLabelDefinition
    branchLength' <- optionMaybe branchLengthDefinition
    pure . NewickNode [] (Just label') $ branchLength'

newickLabelDefinition :: Stream s m Char => ParsecT s u m String
newickLabelDefinition = quotedLabel <|> unquotedLabel

-- | We use a recursive parsing technique to handle the quoted escape sequence
--   of two single quotes ("''") to denote an escaped quotation character 
--   in the quoted label rather than signifying the end of the quoted label
quotedLabel :: Stream s m Char => ParsecT s u m String
quotedLabel = char '\'' *> quotedLabelData <* whitespace
  where 
    quotedLabelData = do
      prefix <- many (noneOf " \r\n'")
      _      <- char '\'' 
      suffix <- optionMaybe (char '\'' <:> quotedLabelData)
      pure $ case suffix of
        Just x  -> prefix ++ x
        Nothing -> prefix

-- | The following characters are not allowed in a newick unquoted label:
--   " \r\n\t\v\b':;,()[]<>"
--   We disallow the '<' & '>' characters in unquoted labels in all newick 
--   file formats because they would interfere with the parsing of Foreset 
--   Extended Newick file types. The '<' & '>' characters are technically
--   allowed in an unquoted newick label according to the Gary Olsen
--   interpretation of the standard Newick format and the Extended Newick
--   format. However, if a user really want to put '<' & '>' characters in
--   a node label, they can always put such characters in a quoted label.
unquotedLabel :: Stream s m Char => ParsecT s u m String
unquotedLabel = many1 (noneOf " \r\n\t\v\b':;,()[]<>") <* whitespace

branchLengthDefinition :: Stream s m Char => ParsecT s u m Double
branchLengthDefinition = symbol (char ':') *> symbol decimal
  where

trimmed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
trimmed x = whitespace *> x <* whitespace

symbol  :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
symbol  x = x <* whitespace

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = try commentDefinition <|> spaces
  where
    commentDefinition :: Stream s m Char => ParsecT s u m ()
    commentDefinition = spaces *> string "[" *> noneOf "]" `manyTill` char ']' <* char ']' <* spaces >>= \_ -> pure ()

joinNonUniqueLabeledNodes :: Stream s m Char => NewickNode -> ParsecT s u m NewickNode
joinNonUniqueLabeledNodes root = joinNonUniqueLabeledNodes' [] root
  where
    -- We first fold over the Newick Tree to collect all labeled nodes and 
    -- combine thier descendant lists. We use this Map of Newick labels to
    -- combined descendant lists for substituting labeled node descendants
    -- in a second pass over the Newick Tree.
    joinedNodes :: Map String [NewickNode]
    joinedNodes = foldl joinNodes empty labeledNodes
      where
        labeledNodes           = filter (isJust . newickLabel) $ toList' root 
        joinNodes :: Map String [NewickNode] -> NewickNode -> Map String [NewickNode]
        joinNodes mapping node = insertWith (++) (fromJust $ newickLabel node) (descendants node) mapping
        toList' :: NewickNode -> [NewickNode]
        toList' = concat . fmap toList' . descendants
    -- When transforming the Newick Tree to the Newick Network by joining 
    -- identically labeled nodes, there exists the possiblily that a directed 
    -- cycle is defined in the tree which will result in infinite recursion 
    -- during the transformation process (and probably erroneous processing
    -- in subsequent graph computations). To prevent this we track previously
    -- processed nodes via a stack to detect cycles. Upon cycle detection we
    -- return a Left value of type Either ParseError NewickNode to represent 
    -- a parse error. It is assumed that cycles are note permitted in our
    -- PhyloGraph data structures.
    joinNonUniqueLabeledNodes' ::  Stream s m Char => [Maybe String] -> NewickNode -> ParsecT s u m NewickNode
    joinNonUniqueLabeledNodes' stack node
      |  isJust label 
      && (not.null) cyclic = fail cycleError
      |  null children     = pure $ newNode []
      |  otherwise         = resultNode children
      where
        label      = newickLabel node
        joinedList = label >>= (`lookup` joinedNodes)
        children   = fromMaybe (descendants node) joinedList
        children'  = sequence . fmap (joinNonUniqueLabeledNodes' stack')
        resultNode = liftM newNode . children'
        newNode x  = NewickNode x label (branchLength node)
        stack'     = label : stack
        cyclic     = dropWhile (/=label) stack
        cycleError = unlines
                   [ "Cycle detected in Newick tree definition" 
                   , show cyclic -- TODO: make the error message prettier!
                   ]

{-
parseAllSuccess :: String -> IO [(String, ParseError)]
parseAllSuccess directory = (fmap fixPaths $ getDirectoryContents (directory++"/"))
                        >>= handleFiles
  where
    handleFiles :: [String] -> IO [(String, ParseError)]
    handleFiles paths = do 
      parsedData <- sequence $ fmap (fmap (getLeft . parseNewickData) . readFile) paths
      return . fmap (id *** fromJust) . Prelude.filter (isJust . snd) $ zip paths parsedData
    getLeft (Left x) = Just x
    getLeft       _  = Nothing
    fixPaths :: [String] -> [String]
    fixPaths = fmap ((directory++"/")++) . Prelude.filter ((/='.').head)
-}
