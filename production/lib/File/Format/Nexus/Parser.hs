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

{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, TypeFamilies #-}

-- TODOs:
-- • Get char weights from assumtions block


module File.Format.Nexus.Parser where

import           Data.Char                (isSpace,toLower)
import           Data.Functor
import qualified Data.List.NonEmpty as NE (head)
import           Data.List.Split          (splitOneOf)
import           Data.Maybe               (isJust)
-- import           Data.Monoid
import qualified Data.Set           as S
import           File.Format.Newick
import           File.Format.Newick.Parser (newickExtendedDefinition)
import           File.Format.Nexus.Data
import           File.Format.Nexus.Partition
import           File.Format.Nexus.Validate
import           File.Format.TransitionCostMatrix.Parser hiding (symbol)
import           Text.Megaparsec  hiding (label, Tokens)
import           Text.Megaparsec.Lexer   (integer)
import           Text.Megaparsec.Prim    (MonadParsec)
import           Text.Megaparsec.Custom

--import Debug.Trace (trace)


-- |
-- Nexus stream parser.
parseNexusStream :: FilePath  -- ^ Used in error message reporting
                 -> String    -- ^ Input stream to be parsed
                 -> Either (ParseError (Token String) Dec) Nexus
parseNexusStream filePath = insertDefaultCharacterNames fileName <$> parse (validateNexusParseResult =<< parseNexus <* eof) filePath
    where
        insertDefaultCharacterNames _ = id
        fileName = last $ splitOneOf "/\\" filePath


-- |
-- An fully parmertric Nexus file parser. An alias for 'nexusFileDefinition'.
parseNexus :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m NexusParseResult
parseNexus = nexusFileDefinition


-- |
-- The high level spacification of the Nexus file format.
nexusFileDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m NexusParseResult
nexusFileDefinition = {-do
    x <- getInput
    trace ("nexusFileDefinition"  ++ show x) $ -}do
    _           <- optional (string' "#NEXUS" <* space)
    _           <- optional . many $ commentDefinition <* space
    (v,w,x,y,z) <- partitionNexusBlocks <$> many nexusBlock
    pure $ NexusParseResult v w x y z


-- TODO: test this
-- why is PROTPARS-example.nex failing? It works with end;, but not with endblock;
-- |
-- Parser for blocks whose content will be ignored.
--
-- One must be wary of false positives, with this parser matching content which
-- should be captured.
ignoredBlockDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m IgnBlock
ignoredBlockDefinition = {-do
    x <- getInput
    trace ("ignoredBlockDefinition"  ++ show x) $ -}do
    line  <- sourceLine . NE.head . statePos <$> getParserState
    title <- many letterChar
    _     <- symbol $ char ';'
    _     <- somethingTill . lookAhead $ symbol blockend
    pure . IgnBlock $ title ++ " at line " ++ show line


-- | 'blockend' is a parser than matched the end of a Nexus block.
--
-- This should be "end;", but "endblock;" is also accepted, as it was used at
-- some point by someone. There is a test in the test suite.
blockend :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m String
blockend = string' "end" <* optional (try $ string' "block") <* char ';'


nexusBlock :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m NexusBlock
nexusBlock = do
        _      <- symbol $ string' "BEGIN"
        block' <- symbol block
        _      <- symbol blockend
        pure block'
    where
        block = choice
            [ CharacterBlock   <$> try (characterBlockDefinition "characters" True)
            , CharacterBlock   <$> try (characterBlockDefinition "unaligned" False)
            , CharacterBlock   <$> try (characterBlockDefinition "data" True) -- data blocks should be aligned
            , TaxaBlock        <$> try taxaBlockDefinition
            , TreesBlock       <$> try treeBlockDefinition
            , AssumptionsBlock <$> try assumptionsBlockDefinition
            , SkippedBlock     <$> try ignoredBlockDefinition
            ]


characterBlockDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> Bool -> m PhyloSequence
characterBlockDefinition which isAlignedSequence = {-do
    x <- getInput
    trace ("characterBlockDefinition "  ++ show x) $ -}do
    _             <- symbol (string' $ which ++ ";")
    (v,w,x,y,z,a) <- partitionSequenceBlock <$> some seqSubBlock
    pure $ PhyloSequence isAlignedSequence v w x y z a which


taxaBlockDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m TaxaSpecification
taxaBlockDefinition = do
    _     <- symbol (string' "taxa;")
    (y,z) <- partitionTaxaBlock <$> many taxaSubBlock
    pure $ TaxaSpecification y z


taxaSubBlock :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m SeqSubBlock
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
assumptionsBlockDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m AssumptionBlock
assumptionsBlockDefinition = {-do
    x <- getInput
    trace ("assumptionsBlockDefinition"  ++ show x) $ -}do
        _                <- symbol (string' "assumptions;")
        (tcms,additives) <- partitionAssumptionBlock <$> many assumptionFieldDef
        pure $ AssumptionBlock tcms additives


-- TODO: complete Add typeset (additive, nonadditive), also capture exset, wtset, deftype and gapmode in options field
assumptionFieldDef :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m AssumptionField
assumptionFieldDef = {-do
    x <- getInput
    trace ("assumptionFieldDef"  ++ show x) $ -}symbol block
    where block =  (TCMMat <$> try tcmMatrixDefinition)
               -- <|> (Add    <$> try )
               <|> (IgnAF  <$> try (ignoredSubBlockDef ';'))


-- | tcmMatrixDefinition expects a string of format
-- USERTYPE myMatrix (STEPMATRIX) =n
-- s s s s
-- k k k k
-- etc.
-- and a StepMatrix, which is some metadata: the matrix name and the cardinality,
-- as well as a TCMParseResult
tcmMatrixDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m StepMatrix
tcmMatrixDefinition = {-do
    x <- getInput
    trace ("\n\ntcmMatrixDefinition "  ++ show x) $ -}do
        _            <- symbol $ string' "usertype"
        matrixName   <- symbol $ somethingTill spaceChar
        _            <- symbol . optional $ try (string' "(stepmatrix)") <|> try (string' "(realmatrix)")
        _            <- symbol $ char '='
        cardinality  <- symbol   integer
        mtxAlphabet  <- symbol $ alphabetLine whitespaceNoNewlines
        assumpMatrix <- symbol $ matrixBlock whitespaceNoNewlines
        _            <- symbol $ char ';'
        pure $ StepMatrix matrixName (fromEnum cardinality) (TCM mtxAlphabet assumpMatrix)


treeBlockDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m TreeBlock
treeBlockDefinition = do
        _     <- symbol (string' "trees;")
        xs    <- many treeFieldDef
        (x,y) <- partitionTreeBlock <$> pure xs
        pure $ TreeBlock x y


-- TODO: Capture values of the StateLabels field, and CharStateLabels field.
seqSubBlock :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m SeqSubBlock
seqSubBlock = {-do
    x <- getInput
    trace ("seqSubBlock" ++ (show x)) $ -}symbol block
    where
        block = choice
            [ Dims        <$> try dimensionsDefinition
            , Format      <$> try formatDefinition
            , Eliminate   <$> try (stringDefinition "eliminate")
            , Matrix      <$> try seqMatrixDefinition
            , Taxa        <$> try (stringListDefinition "taxlabels")
            , CharLabels  <$> try (stringListDefinition "charlabels")
            , IgnSSB      <$> try (ignoredSubBlockDef ';')
            ]


-- |
-- Parses the dimension format fields of a Character block.
--
-- Correctly matches and discards information which is not captured.
dimensionsDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m DimensionsFormat
dimensionsDefinition = {-do
        x         <- getInput
        trace ("**dimensionsDefinition:  " ++ show x) $ -}do
        _         <- symbol   $ string' "dimensions"
        newTaxa'  <- optional $ try (symbol (string' "newTaxa"))
        _         <- optional $ try (symbol (string' "nTax"))
        _         <- optional $ try (symbol (char '='))
        numTaxa'  <- optional $ try (symbol integer)
        _         <- optional $ symbol ((string' "nchar") *> optional (char' 's'))
        _         <- optional $ symbol (char '=')
        charCount <- optional $ try (symbol integer)
        _         <- symbol   $ char ';'
        pure $ DimensionsFormat (isJust newTaxa')
                                (maybe 0 fromEnum numTaxa')
                                (maybe 0 fromEnum charCount)


-- | formatDefinition tests an input String. If that String passes the (implicit) definition
-- of a format statement in the characters block or unaligned block of a Nexus file, it returns
-- a parse of the String. A well-formed input string will start with the word "format" followed by
-- a space-delimited list of words, each of which can be successfully parsed by
-- charFormatFieldDef, and end with a semi-colon.
-- TODO: An incomplete test exists in the test suite.
formatDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m CharacterFormat
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
charFormatFieldDef :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m [CharFormatField]
charFormatFieldDef = {-do
        x <- getInput
        trace ("many charFormatFieldDef"  ++ show x) $ -}do
        block' <- many $ symbol block
        pure block'
    where
        block = choice
            [ CharDT      <$> try (string' "datatype" *> symbol (char '=') *> charDTDefinition)
            , SymStr      <$> try (quotedStringDefinition "symbols"    )
            , Transpose   <$> try (booleanDefinition      "transpose"  )
            , Interleave  <$> try (booleanDefinition      "interleave" )
            , Tokens      <$> try (booleanDefinition      "tokens"     )
            , EqStr       <$> try (quotedStringDefinition "equate"     )
            , MissStr     <$> try (stringDefinition       "missing"    )
            , GapChar     <$> try (stringDefinition       "gap"        )
            , MatchChar   <$> try (stringDefinition       "matchchar"  )
            , Items       <$> try (stringDefinition       "items"      )
            , RespectCase <$> try (booleanDefinition      "respectcase")
            , Unlabeled   <$> try (booleanDefinition      "nolabels"   )
            , IgnFF       <$> try (ignoredSubBlockDef     ' '          )
            ]
        charDTDefinition = choice
                         [ string' "standard"   $> Standard
                         , string' "dna"        $> DNA
                         , string' "rna"        $> RNA
                         , string' "nucleotide" $> Nucleotide
                         , string' "protein"    $> Protein
                         , string' "continuous" $> Continuous
                         ] <?> "\nDatatype is not recognized."

-- |
-- Parses one of the two caputured field values present in a tree block.
treeFieldDef :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m TreeField
treeFieldDef = choice
    [ Translation <$> try (delimitedStringListDefinition "translate" ',')
    , Tree        <$> try treeDefinition
    , IgnTF       <$> try (ignoredSubBlockDef ';')
    ]


-- | booleanDefinition takes a string of format KEYWORD;
-- and returnsTreeField True if it succeeds in matching. The semicolon is not captured by this fn.
-- A test exists in the test suite.
booleanDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> m Bool
booleanDefinition blockTitle = {-do
        x <- getInput
        trace (("booleanDefinition " ++ blockTitle)  ++ show x) $-}
    symbol (string' blockTitle) $> True

-- | stringDefinition takes a string of format TITLE=value;
-- and returns the value. The semicolon is not captured by this fn.
-- A test exists in the test suite.
stringDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> m String
stringDefinition blockTitle = do
    _     <- symbol $ string' blockTitle
    _     <- symbol $ char '='
    symbol (notKeywordWord "\"" <?> "Word that is not a Nexus keyword")


-- | quotedStringDefinition takes a string of format TITLE="value1 value2 ...";
-- and returns a list of the value(s). The values are separated by whitespace.
-- The semicolon is not captured by this fn.
-- Fails gracefully if the close quote is missing.
-- A test exists in the test suite.
-- TODO?: This doesn't work if they leave off the opening quote mark.
quotedStringDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> m (Either String [String])
quotedStringDefinition blockTitle = {-do
    x <- getInput
    trace (("some quotedStringDefinition " ++ blockTitle)  ++ show x) $ -}do
    _     <- symbol $ string' blockTitle
    _     <- symbol $ char '='
    _     <- symbol $ char '"'
    value <- some $ symbol (notKeywordWord "\"" <?> "Word that is not a Nexus keyword")
    close <- optional $ char '"'
    pure $ if isJust close
           then Right value
           else Left (blockTitle ++ " missing closing quote.")
    -- _ <- symbol $ char '"'
    --pure $ Right value


-- | stringListDefinition is similar to quotedStringDefinition, but in this case the format is
-- TITLE val1 val2 val3 ...; In other words, there is no '=' and the list of values is whitepace-separated.
-- Those values are captured and returned.
stringListDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> m [String]
stringListDefinition label = {-do
    x <- getInputs
    trace (("many stringListDefinition " ++ label)  ++ show x) $ -}do
    _        <- symbol (string' label)
    theItems <- many . symbol $ notKeywordWord ""
    _        <- symbol $ char ';'
    pure theItems


-- | delimitedStringListDefinition is similar to stringListDefinition, but in this case the format is
-- TITLE val1, val2, val3, ...; Again, there is no '='' after the title of the block. However, in this
-- case values are separated by a Char which is passed in as an argument. (In the given example the separator
-- is ','. The values are captured and returned.
delimitedStringListDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> Char -> m [String]
delimitedStringListDefinition label delimiter = {-do
    x <- getInput
    trace (("delimitedStringListDefinition " ++ label)  ++ show x) $ -}do
    _        <- symbol (string' label)
    theItems <- many (noneOf $ delimiter : ";") `sepBy` trimmed (char delimiter)
    _        <- symbol $ char ';'
    pure theItems


-- | treeDefinition parses a 'String' of the format "TREE <label>=<newick tree/forest>".
-- and returns the tuple of (<label>, '[NewickForest']). Label consists on one or more
-- non-space, non-equal-sign characters. For the proper definition of a 'NewickForest'
-- see the module for the Newick parser.
treeDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m (String, NewickNode)
treeDefinition = do
    _      <- symbol $ string' "tree"
    label  <- symbol $ somethingTill (char '=' <|> spaceChar)
    _      <- symbol $ char '='
    newick <- symbol newickExtendedDefinition -- newickStreamParser
    pure (label, newick)


seqMatrixDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m [String]
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
ignoredSubBlockDef :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => Char -> m String
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


-- |
-- Parses the provided definition and consumes any leading /and/ and trailing
-- whitespace.
trimmed :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m a -> m a
trimmed x = whitespace *> x <* whitespace


-- |
-- Parses the provided definition and consumes any trailing whitespace.
symbol :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m a -> m a
symbol x = x <* whitespace


-- | whitespace is any combination (zero or more) of whitespace chars (" \t\n\r\v\f") and comments, which in Nexus files
-- are delimited by square brackets.
-- TODO: Since this accepts the empty string, it's difficult to test....
whitespace :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m ()
whitespace = (space *> optional (try . some $ commentDefinition *> space) $> ())
          <?> "comments or whitespace"


-- |
-- Consumes whitspace in the stream without moving to the next line, unless
-- there were newlines present in a comment contained in the whitespace.
--
-- Consumes whitespace (including multi-line comments) but not newlines outside
-- of a comment definition.
whitespaceNoNewlines :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m ()
whitespaceNoNewlines = (inlineSpace *> optional (try . some $ commentDefinition *> inlineSpace) $> ())
          <?> "comments or non-newline whitespace"


-- |
-- Capture the contents of a comment
commentDefinition :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => m String
commentDefinition = comment (string "[") (string "]")


-- |
-- Consume one or more spaces.
space1 :: (MonadParsec e s m , Token s ~ Char {- Show s -}) => m ()
space1 = skipSome spaceChar


-- | notKeywordWord takes two strings as input and returns a String.
-- The first argument is a list of characters which
-- should not appear in any returned value (i.e. a list of delimiters).
-- The second argument is a string to parse.
-- In addition, it checks to make sure that the output is not a Nexus keywords. If the output is
-- a keyword, the fn fails with an error message.
notKeywordWord :: (MonadParsec e s m, Token s ~ Char {- , Show s -}) => String -> m String
notKeywordWord avoidChars = do
    word <- lookAhead nextWord
    if (toLower <$> word) `elem` nexusKeywords
    then fail $ "Unexpected keyword '" ++ word ++ "', perhaps you are missing a semicolon?"
    else nextWord
  where
    nextWord = some . try $ satisfy (\x -> x `notElem` (';' : avoidChars) && not (isSpace x))

-- |
-- A collection of nexus keywords that must be queried against during parsing to
-- prevent erroneously consuming keywords as data content in another parser
-- operating on a malformed stream.
nexusKeywords :: S.Set String
nexusKeywords = S.fromList
    [ "ancstates"
    , "assumptions"
    , "begin"
    , "changeset"
    , "characters"
    , "charlabels"
    , "charpartition"
    , "charset"
    , "charstatelabels"
    , "codeorder"
    , "codeset"
    , "codons"
    , "data"
    , "datatype"
    , "deftype"
    , "diagonal"
    , "dimensions"
    , "distances"
    , "eliminate"
    , "end"
    , "endblock"
    , "equate"
    , "exset"
    , "extensions"
    , "format"
    , "gap"
    , "interleave"
    , "items"
    , "labels"
    , "matchchar"
    , "matrix"
    , "missing"
    , "nchar"
    , "newtaxa"
    , "nodiagonal"
    , "nolabels"
    , "notes"
    , "notokens"
    , "ntax"
    , "options"
    , "picture"
    , "respectcase"
    , "sets"
    , "statelabels"
    , "statesformat"
    , "symbols"
    , "taxa"
    , "taxlabels"
    , "taxpartition"
    , "taxset"
    , "text"
    , "tokens"
    , "translate"
    , "transpose"
    , "tree"
    , "treepartition"
    , "trees"
    , "treeset"
    , "triangle"
    , "typeset"
    , "unaligned"
    , "usertype"
    , "wtset"
    ]

