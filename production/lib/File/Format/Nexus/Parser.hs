-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nexus.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing Nexus files.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse, FlexibleContexts #-}

-- TODOs:
-- • Attempt to optimize sequence reading: concatting, character replacement, etc.
-- • Reconceive and reorder current validations. Keep in mind next bullet point.
-- • Check for info on spaces in seqs. Remove them from non-continuous data.
-- • Verify character metadata, especially alphabets
-- • Verify step matrices
-- • deal with special chars in step matrices
-- • Split verification & parsing into two modules, maybe three (one for data types)
-- • check for and eliminate thusly noted characters (in at least two places?)
-- • update output datatypes: nest vectors, add character metadata, trees, step matrices
-- • replace equate chars 
-- • ignore case
-- • check alignment and length of aligned blocks, check alphabet, think about 12 and 20, below, make each sequence a vector, not a list
-- • Something else?

module File.Format.Nexus.Parser where

import           Data.Char              (isSpace,toLower)
import           Data.Maybe             (isJust)
import qualified Data.Set as S
--import Debug.Trace
import           File.Format.Newick
import           File.Format.Nexus.Data
import           File.Format.Nexus.Partition
import           File.Format.Nexus.Validate
import           File.Format.TransitionCostMatrix.Parser hiding (symbol)
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Lexer  (integer)
import           Text.Megaparsec.Prim   (MonadParsec)
import           Text.Megaparsec.Custom

parseNexusStream :: String -> Either ParseError Nexus
parseNexusStream = parse (validateNexusParseResult =<< parseNexus <* eof) "PCG encountered a Nexus file parsing error it could not overcome:"






parseNexus :: (Show s, MonadParsec s m Char) => m NexusParseResult
parseNexus = nexusFileDefinition

nexusFileDefinition :: (Show s, MonadParsec s m Char) => m NexusParseResult
nexusFileDefinition = {-do
    x <- getInput
    trace ("nexusFileDefinition"  ++ show x) $ -}do
    _           <- string' "#NEXUS"
    _           <- space
    _           <- optional $ many $ commentDefinition <* space
    (v,w,x,y,z) <- partitionNexusBlocks <$> many nexusBlock
    pure $ NexusParseResult v w x y z

-- TODO: test this 
-- why is PROTPARS-example.nex failing? It works with end;, but not with endblock;
ignoredBlockDefinition :: (Show s, MonadParsec s m Char) => m IgnBlock
ignoredBlockDefinition = {-do
    x <- getInput
    trace ("ignoredBlockDefinition"  ++ show x) $ -}do
    line  <- sourceLine . statePos <$> getParserState
    title <- many letterChar
    _     <- symbol $ char ';'
    _     <- somethingTill $ lookAhead $ symbol blockend
    pure $ IgnBlock $ title ++ " at line " ++ show line

-- | blockend is a parser than matched the end of a Nexus block.
-- this should be "end;", but "endblock;" is also accepted, as it was used -- at some point by someone
-- There is a test in the test suite.
blockend :: (Show s, MonadParsec s m Char) => m String
blockend = string' "end" <* optional (try $ string' "block") <* char ';'

nexusBlock :: (Show s, MonadParsec s m Char) => m NexusBlock
nexusBlock = do
        _      <- symbol $ string' "BEGIN"
        block' <- symbol block
        _      <- symbol $ blockend
        pure block'
    where
        block =  (CharacterBlock   <$> try (characterBlockDefinition "characters" True))
             <|> (CharacterBlock   <$> try (characterBlockDefinition "unaligned" False))
             <|> (CharacterBlock   <$> try (characterBlockDefinition "data" True)) -- data blocks should be aligned
             <|> (TaxaBlock        <$> try taxaBlockDefinition)
             <|> (TreesBlock       <$> try treeBlockDefinition)
             <|> (AssumptionsBlock <$> try assumptionsBlockDefinition)
             <|> (SkippedBlock     <$> try ignoredBlockDefinition)

characterBlockDefinition :: (Show s, MonadParsec s m Char) => String -> Bool -> m PhyloSequence
characterBlockDefinition which isAlignedSequence = {-do
    x <- getInput
    trace ("characterBlockDefinition "  ++ show x) $ -}do
    _           <- symbol (string' $ which ++ ";")
    (v,w,x,y,z) <- partitionSequenceBlock <$> some seqSubBlock
    pure $ PhyloSequence isAlignedSequence v w x y z which

taxaBlockDefinition :: (Show s, MonadParsec s m Char) => m TaxaSpecification
taxaBlockDefinition = {-do
    x <- getInput
    trace ("taxaBlockDefinition"  ++ show x) $ -}do
    _     <- symbol (string' "taxa;")
    (y,z) <- partitionTaxaBlock <$> many taxaSubBlock
    pure $ TaxaSpecification y z

taxaSubBlock :: (Show s, MonadParsec s m Char) => m SeqSubBlock
taxaSubBlock = {-do
    x <- getInput
    trace ("many taxaSubBlock"  ++ show x) $ -}do
        _      <- whitespace
        block' <- symbol block
        pure block'
    where
        block =  (Dims   <$> try dimensionsDefinition)
             <|> (Taxa   <$> try (stringListDefinition "taxlabels"))
             <|> (IgnSSB <$> try (ignoredSubBlockDef ';'))

-- | assumptionsBlockDefinition 
assumptionsBlockDefinition :: (Show s, MonadParsec s m Char) => m AssumptionBlock
assumptionsBlockDefinition = {-do
    x <- getInput
    trace ("assumptionsBlockDefinition"  ++ show x) $ -}do
        _   <- symbol (string' "assumptions;")
        goodStuff <- partitionAssumptionBlock <$> many assumptionFieldDef
        pure $ AssumptionBlock goodStuff

assumptionFieldDef :: (Show s, MonadParsec s m Char) => m AssumptionField
assumptionFieldDef = {-do
    x <- getInput
    trace ("assumptionFieldDef"  ++ show x) $ -}symbol block
    where block =  (TCMMat <$> try tcmMatrixDefinition)
               <|> (IgnAF  <$> try (ignoredSubBlockDef ';'))

-- | tcmMatrixDefinition expects a string of format 
-- USERTYPE myMatrix (STEPMATRIX) =n
-- s s s s
-- k k k k
-- etc.
-- and a StepMatrix, which is some metadata: the matrix name and the cardinality,
-- as well as a TCMParseResult
tcmMatrixDefinition :: (Show s, MonadParsec s m Char) => m StepMatrix
tcmMatrixDefinition = {-do
    x <- getInput
    trace ("\n\ntcmMatrixDefinition "  ++ show x) $ -}do
        _            <- symbol $ string' "usertype"
        matrixName   <- symbol $ somethingTill spaceChar
        _            <- symbol $ optional $ try (string' "(stepmatrix)") <|> try (string' "(realmatrix)")
        _            <- symbol $ char '='
        cardinality  <- symbol $ integer 
        mtxAlphabet  <- symbol $ alphabetLine whitespaceNoNewlines
        assumpMatrix <- symbol $ matrixBlock whitespaceNoNewlines
        _            <- symbol $ char ';'
        pure $ StepMatrix matrixName (fromEnum cardinality) (TCM mtxAlphabet assumpMatrix)

treeBlockDefinition :: (Show s, MonadParsec s m Char) => m TreeBlock
treeBlockDefinition = {-do
    x <- getInput
    trace ("\n\ntreeBlockDefinition "  ++ show x) $ -}do
        _     <- symbol (string' "trees;")
        (x,y) <- partitionTreeBlock <$> many treeFieldDef
        pure $ TreeBlock x y

seqSubBlock :: (Show s, MonadParsec s m Char) => m SeqSubBlock
seqSubBlock = {-do
    x <- getInput
    trace ("seqSubBlock" ++ (show x)) $ -}symbol block
    where
        block =  (Dims      <$> try dimensionsDefinition)
             <|> (Format    <$> try formatDefinition)
             <|> (Eliminate <$> try (stringDefinition "eliminate"))
             <|> (Matrix    <$> try seqMatrixDefinition)
             <|> (Taxa      <$> try (stringListDefinition "taxlabels"))
             <|> (IgnSSB    <$> try (ignoredSubBlockDef ';'))

dimensionsDefinition :: (Show s, MonadParsec s m Char) => m DimensionsFormat
dimensionsDefinition = {-do 
        x         <- getInput 
        trace ("**dimensionsDefinition:  " ++ show x) $ -}do
        _         <- symbol (string' "dimensions")
        newTaxa'  <- optional (try (symbol (string' "newTaxa")))
        _         <- optional (try (symbol (string' "nTax")))
        _         <- optional $ try (symbol (char '='))
        numTaxa'  <- optional $ try (symbol integer)
        _         <- optional $ symbol (string' "nchar")
        _         <- optional $ symbol (char '=')
        charCount <- optional $ try (symbol integer)
        _         <- symbol $ char ';'
        pure $ DimensionsFormat (isJust newTaxa')
                                (maybe 0 fromEnum numTaxa')
                                (maybe 0 fromEnum charCount)

-- | formatDefinition tests an input String. If that String passes the (implicit) definition
-- of a format statement in the characters block or unaligned block of a Nexus file, it returns
-- a parse of the String. A well-formed input string will start with the word "format" followed by
-- a space-delimited list of words, each of which can be successfully parsed by 
-- charFormatFieldDef, and end with a semi-colon.
-- TODO: An incomplete test exists in the test suite.
formatDefinition :: (Show s, MonadParsec s m Char) => m CharacterFormat
formatDefinition = {-do
        x <- getInput
        trace ("\n\nmany formatDefinition "  ++ show x) $ -}do
        _                         <- symbol (string' "format")
        (o,p,q,r,s,t,u,v,w,x,y,z) <- partitionCharFormat <$> charFormatFieldDef
        _                         <- symbol $ char ';'
        pure $ CharacterFormat o p q r s t u v w x y z

-- | charFormatFieldDef takes a String and attempts to parse it into multiple CharFormatFields
-- the return type is [CharFormatField]. The parse fails if any part of the string cannot be parsed
-- by any of the sub-parsers, or if any of those sub-parsers fails.
-- A test exists in the test suite, although only false positives are tested for, not false negatives.
-- I deemed this good enough, since each of the called fns is well-tested, and the calling fn is, as well.
charFormatFieldDef :: (Show s, MonadParsec s m Char) => m [CharFormatField]
charFormatFieldDef = {-do
        x <- getInput
        trace ("many charFormatFieldDef"  ++ show x) $ -}do
        block' <- many $ symbol block
        pure block'
    where
        block =  (CharDT      <$> try (stringDefinition       "datatype"   ))
             <|> (SymStr      <$> try (quotedStringDefinition "symbols"    ))
             <|> (Transpose   <$> try (booleanDefinition      "transpose"  ))
             <|> (Interleave  <$> try (booleanDefinition      "interleave" ))
             <|> (Tokens      <$> try (booleanDefinition      "tokens"     ))
             <|> (EqStr       <$> try (quotedStringDefinition "equate"     ))
             <|> (MissStr     <$> try (stringDefinition       "missing"    ))
             <|> (GapChar     <$> try (stringDefinition       "gap"        ))
             <|> (MatchChar   <$> try (stringDefinition       "matchchar"  ))
             <|> (Items       <$> try (stringDefinition       "items"      ))
             <|> (RespectCase <$> try (booleanDefinition      "respectcase"))
             <|> (Unlabeled   <$> try (booleanDefinition      "nolabels"   ))
             <|> (IgnFF       <$> try (ignoredSubBlockDef     ' '          ))


treeFieldDef :: (Show s, MonadParsec s m Char) => m TreeField
treeFieldDef = do
        block' <- block
        pure block'
    where
        block =  (Translation <$> try (delimitedStringListDefinition "translate" ','))
             <|> (Tree        <$> try treeDefinition)
             <|> (IgnTF       <$> try (ignoredSubBlockDef ';'))


-- | booleanDefinition takes a string of format KEYWORD;
-- and returns True if it succeeds in matching. The semicolon is not captured by this fn.
-- A test exists in the test suite.
booleanDefinition :: (Show s, MonadParsec s m Char) => String -> m Bool
booleanDefinition blockTitle = {-do
        x <- getInput
        trace (("booleanDefinition " ++ blockTitle)  ++ show x) $-} symbol (string' blockTitle) *> pure True

-- | stringDefinition takes a string of format TITLE=value;
-- and returns the value. The semicolon is not captured by this fn.
-- A test exists in the test suite.
stringDefinition :: (Show s, MonadParsec s m Char) => String -> m String
stringDefinition blockTitle = do
    _     <- symbol $ string' blockTitle
    _     <- symbol $ char '='
    value <- symbol $ notKeywordWord ""
    pure value

-- | quotedStringDefinition takes a string of format TITLE="value1 value2 ...";
-- and returns a list of the value(s). The values are separated by whitespace.
-- The semicolon is not captured by this fn.
-- Fails gracefully if the close quote is missing.
-- A test exists in the test suite.
-- TODO?: This doesn't work if they leave off the opening quote mark.
quotedStringDefinition :: (Show s, MonadParsec s m Char) => String -> m (Either String [String])
quotedStringDefinition blockTitle = {-do
    x <- getInput
    trace (("some quotedStringDefinition " ++ blockTitle)  ++ show x) $ -}do
    _     <- symbol (string' blockTitle)
    _     <- symbol $ char '='
    _     <- symbol $ char '"'
    value <- some $ symbol (notKeywordWord "\"" <?> "Word that is not a Nexus keyword")
    close <- optional $ char '"'
    pure $ if isJust close
           then Right value
           else Left (blockTitle ++ " missing closing quote.") 
    -- _ <- symbol $ char '"'
    --pure $ Right value

stringListDefinition :: (Show s, MonadParsec s m Char) => String -> m [String]
stringListDefinition label = {-do
    x <- getInputs
    trace (("many stringListDefinition " ++ label)  ++ show x) $ -}do
    _        <- symbol (string' label)
    theItems <- many $ symbol $ notKeywordWord ""
    _        <- symbol $ char ';'
    pure theItems

delimitedStringListDefinition :: (Show s, MonadParsec s m Char) => String -> Char -> m [String]
delimitedStringListDefinition label delimiter = {-do
    x <- getInput
    trace (("delimitedStringListDefinition " ++ label)  ++ show x) $ -}do
    _        <- symbol (string' label)
    theItems <- many (noneOf $ delimiter : ";") `sepBy` char delimiter
    _        <- symbol $ char ';'
    pure theItems

-- | treeDefinition parses a 'String' of the format "TREE <label>=<newick tree/forest>".
-- and returns the tuple of (<label>, '[NewickForest']). Label consists on one or more
-- non-space, non-equal-sign characters. For the proper definition of a 'NewickForest'
-- see the module for the Newick parser.
treeDefinition :: (Show s, MonadParsec s m Char) => m (String, [NewickForest])
treeDefinition = {-do
    x <- getInput
    trace ("treeDefinition"  ++ show x) $ -}do
    _      <- symbol $ string' "tree"
    label  <- symbol $ somethingTill (char '=' <|> spaceChar)
    _      <- symbol $ char '='
    newick <- symbol newickStreamParser
    pure (label, newick)

seqMatrixDefinition :: (Show s, MonadParsec s m Char) => m [String]
seqMatrixDefinition = {-do
    x <- getInput
    trace ("\n\nseqMatrixDefinition "  ++ show x) $ -}do
    _         <- symbol $ string' "matrix"
    goodStuff <- some   $ somethingTill c <* c
    _         <- symbol $ char ';'
    pure $ filter (/= "") goodStuff
    where 
        c = whitespaceNoNewlines *> (char ';' <|> endOfLine) <* whitespace

-- | ignoredSubBlockDef takes any string that terminates with
-- the passed end character, a semicolon or "end;". It returns that string up to, but
-- not including, whatever the terminating char is. Also fails if the input is "end;"
-- A test exists in the test suite.
ignoredSubBlockDef :: (Show s, MonadParsec s m Char) => Char -> m String
ignoredSubBlockDef endChar = {-do
    x <- getInput
    trace (("\n\nignoredSubBlockDef endChar: " ++ [endChar] ++ " ")  ++ show x) $ -}do
    _     <- notFollowedBy (space *> blockend) <?> "something other than end of block"
    stuff <- somethingTill stopMark
    _     <- stopMark
    pure stuff
  where
    stopMark = symbol $ char ';' <|> char' endChar



------------------------
-- Utility fns --
-- TODO: Are these used?

trimmed :: (Show s, MonadParsec s m Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace


symbol :: (Show s, MonadParsec s m Char) => m a -> m a
symbol x = x <* whitespace

-- | whitespace is any combination (zero or more) of whitespace chars (" \t\n\r\v\f") and comments, which in Nexus files
-- are delimited by square brackets.
-- TODO: Since this accepts the empty string, it's difficult to test....
whitespace :: (Show s, MonadParsec s m Char) => m ()
whitespace = (space *> optional (try $ some $ commentDefinition *> space) *> pure ())
          <?> "comments or whitespace"

whitespaceNoNewlines :: (Show s, MonadParsec s m Char) => m ()
whitespaceNoNewlines = (inlineSpace *> optional (try $ some $ commentDefinition *> inlineSpace) *> pure ())
          <?> "comments or non-newline whitespace"

commentDefinition :: (Show s, MonadParsec s m Char) => m String
commentDefinition = comment (string "[") (string "]")

space1 :: (Show s, MonadParsec s m Char) => m ()
space1 = skipSome spaceChar

nexusKeywords :: S.Set String
nexusKeywords = S.fromList ["ancstates", "assumptions", "begin", "changeset", "characters", "charlabels", "charpartition", "charset", "charstatelabels", "codeorder", "codeset", "codons", "data", "datatype", "deftype", "diagonal", "dimensions", "distances", "eliminate", "end", "endblock", "equate", "exset", "extensions", "format", "gap", "interleave", "items", "labels", "matchchar", "matrix", "missing", "nchar", "newtaxa", "nodiagonal", "nolabels", "notes", "notokens", "ntax", "options", "picture", "respectcase", "sets", "statelabels", "statesformat", "symbols", "taxa", "taxlabels", "taxpartition", "taxset", "text", "tokens", "translate", "transpose", "tree", "treepartition", "trees", "treeset", "triangle", "typeset", "unaligned", "usertype", "wtset"]

-- | notKeywordWord takes two strings as input and returns a String.
-- The first argument is a list of characters which
-- should not appear in any returned value (i.e. a list of delimiters).
-- The second argument is a string to parse.
-- In addition, it checks to make sure that the output is not a Nexus keywords. If the output is
-- a keyword, the fn fails with an error message.
notKeywordWord :: (Show s, MonadParsec s m Char) => String -> m String
notKeywordWord avoidChars = do
    word <- lookAhead nextWord
    if (toLower <$> word) `S.member` nexusKeywords
    then fail $ "Unexpected keyword '" ++ word ++ "', perhaps you are missing a semicolon?"
    else nextWord
  where
    nextWord = some $ try $ satisfy (\x -> x `notElem` (';' : avoidChars) && not (isSpace x))


