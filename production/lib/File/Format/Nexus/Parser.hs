{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Parser where

import Data.Char (isSpace,toLower,toUpper)
import Data.Either (lefts)
import qualified Data.Map.Lazy as M
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Set as S
import Debug.Trace
import Safe
import Text.Parsec hiding (label)
import Text.Parsec.Custom
import qualified Data.Vector as V

data ParseResult = ParseResult [PhyloSequence] [TaxaSpecification] [TreeBlock] deriving (Show)

data NexusBlock
   = TaxaBlock      TaxaSpecification
   | CharacterBlock PhyloSequence
   | TreesBlock     TreeBlock
   | IgnoredBlock   String
   deriving (Show)

-- | DimensionsFormat is format of dimensions field in characters and unaligned nexus blocks.
-- It could also actually be used for the dimensions in the taxa block, with newTaxa = False
-- and numChars = 0
data DimensionsFormat
   = DimensionsFormat
   { newTaxa  :: Bool
   , numTaxa  :: Int
   , numChars :: Int
   } deriving (Show)

-- | Phylosequence is general sequence type, to be used for both characters (aligned) and unaligned blocks.
data PhyloSequence
   = PhyloSequence
   { aligned       :: Bool
   , matrix        :: [String]
   , format        :: [CharacterFormat]
   , charDims      :: [DimensionsFormat] -- holds length of sequence, as well as info on new taxa
   , elims         :: [String]
   , seqTaxaLabels :: [[String]]
   } deriving (Show)

data TreeBlock
   = TreeBlock
   { translate :: [[String]]
   , trees     :: [(String,String)]
   } deriving (Show)

-- | SeqSubBlock is list of fields available in sequence blocks. It's set up as an enumeration
-- so that it can be "looped" over when parsing the sequence blocks, as the order of the fields
-- is not documented.
data SeqSubBlock
   = Matrix          String
   | Format          CharacterFormat
   | Dims            DimensionsFormat
   -- | Items           ItemType
   | Eliminate       String
   | CharStateLabels [CharStateFormat]
   | Ignored         String
   | Taxa            [String]
   deriving (Show)

data TaxaSpecification
   = TaxaSpecification
   { taxaDims   :: Int
   , taxaLabels :: [String]
   } deriving (Show)

-- | The different subfields of the Format field in the sequence blocks.
-- As with SeqSubBlock, listed simply so that it can be "looped" over. Will eventually be
-- coverted to CharacterFormat data type for export
data CharFormatField
   = CharDT      String
   | SymStr      (Either String [String]) -- the list of symbols
   | EqStr       (Either String [String]) -- the equate (symbol -> symbols) will be processed into Map Char String
   | MissStr     String -- "missing" char
   | GapChar     String -- gap char
   | MatchChar   String -- "match" char
   | Items       String
   | RespectCase Bool   -- should the case of the characters be respected?
   | Tokens      Bool
   | Transpose   Bool
   | Interleave  Bool
   | Unlabeled   Bool   -- if matrix is unlabeled, in which case first token in each line is a char
   | IgnFF       String -- for non-standard inputs, plus notokens, which is the default anyway
   deriving (Show)


data TreeField
   = Translation [String]
   | Tree        (String,String)
   | IgnTF       String

data CharStateFormat
   = CharStateFormat
   { charNum   :: Int
   , charName  :: String
   , stateName :: [String]
   } deriving (Show)

data CharacterFormat
   = CharacterFormat
   { charDataType :: String
   , symbols      :: (Either String [String])
   , equate       :: (Either String [String])
   , missing      :: String
   , gap          :: String
   , matchChar    :: String
   , items        :: String
   , respectCase  :: Bool
   , areTokens    :: Bool
   , transpose    :: Bool
   , interleave   :: Bool
   , unlabeled    :: Bool
   } deriving (Show)

data CharDataType = Standard | DNA | RNA | Nucleotide | Protein | Continuous deriving (Read, Show)

data Nexus
   = Nexus
   { taxa :: [String]
   , characters :: [M.Map String [[String]]]
   --, characters :: SequenceBlock
   } deriving (Show)

data SequenceBlock
   = SequenceBlock
   { charType  :: CharDataType
   , alphabet  :: String
   , isAligned :: Bool
   , seqs      :: [V.Vector (String, [String])]
   } deriving (Show)

parseNexusStream :: String -> Either ParseError Nexus
parseNexusStream = parse (validateParseResult =<< parseNexus <* eof) "PCG encountered a Nexus file parsing error it could not overcome:"

-- Because some errors are dependent on passing some other validation, they've been split into
-- dependant and independant errors. Errors are enumerated below, along with their dependancies.
-- Comments are included in the code that explain which of the below errors are caught at various points.
-- Note that nos. 4--6, below, are only dependent on 3, so are simply done as nested ifs. This may
-- or may not be best practice.                                                             coded as
-- Errors currently caught: # error                                       dependency   inde- or de- pendant    finished
--  1. Too many taxa blocks                                                    -              indep              done
--  2. too many sequence blocks                                                -              indep              done
--     There are actually two subcases here:
--        too many aligned, too many unaligned
--  3. No sequence blocks                                                      -              indep              done
--  4. No matrix in some sequence block                                        3              indep              done
--  5. No dimensions in some sequence block                                    3              indep              done
--  6. No taxa block _and_ no new taxa specified in sequence blocks            3              indep              done
--  7. "newtaxa" keyword in seq block, but no taxa actually spec'ed            3                dep              done
--  8. "newtaxa" keywork in seq block, but ntaxa missing                       3,5              dep              done
--  9. "nolabels" but labels actually present--subsumed under 10 and 11        3                dep
-- 10. Matrix has alphabet values not specified                                3,4              dep
-- 11. Aligned block not Aligned                                               3,4              dep
-- 12. Matrix has non-spec'ed taxa                                             1,3,4,5          dep
-- 13. Matrix interleaved, but some blocks missing taxa                        1,3,4,5          dep
--     ---for aligned matrices, caught by 10
--     ---for unaligned, bother catching?
-- 14. Matrix interleaved, unlabeled, but length not a multiple of # of taxa   1,3,4            dep              done
-- 15. Character count is incorrect                                            3,4,5            dep
-- 16. Taxa count is incorrect                                                 1,3,5            dep              done
-- 17. "nolabels" but there is a taxa block and newtaxa in seq block           1,3,7,8          dep
-- 18. Missing semicolons                                                       -             indep              caught in parse
-- 19. Equate or symbols strings missing their closing quotes                  3                dep              done
validateParseResult :: ParseResult -> Parsec s u Nexus
validateParseResult (ParseResult sequences taxas trees)
  | not (null independentErrors) = fails independentErrors
  | not (null dependentErrors)   = fails dependentErrors
  | otherwise                  = pure $ Nexus taxaLst (alignedTaxaSeqMap ++ unalignedTaxaSeqMap)
  where
        alignedTaxaSeqMap = getSeqFromMatrix (getBlock "aligned" sequences) taxaLst
        dependentErrors = catMaybes $ incorrectTaxaCount : (missingCloseQuotes ++ seqTaxaCountErrors ++ interleaveErrors)
        equates = foldr (\x acc -> (getEquates x) : acc) [] sequences
        independentErrors = catMaybes $ noTaxaError : multipleTaxaBlocks : sequenceBlockErrors
        f [x] = Just x
        f _   = Nothing

        incorrectTaxaCount = f taxas >>= \(TaxaSpecification num taxons) -> if num /= length taxons
                then Just $ "Incorrect number of taxa in taxa block." ++ (show num) ++ " " ++ (show taxons) -- half of error 16
                else Nothing
        interleaveErrors = foldr (\x acc -> (findInterleaveError taxaLst x) : acc) [] sequences -- error 14
        matrixDimsErrors = (foldr (\x acc -> (matrixMissing x) : acc) [] sequences) ++
                           (foldr (\x acc -> (dimsMissing x) : acc) [] sequences)
        missingCloseQuotes = (map (\x -> Just x) (lefts equates)) ++ (map (\x -> Just x) (lefts symbols')) -- error 19
        multipleTaxaBlocks = case taxas of
                            (_:_:_) -> Just "Multiple taxa blocks supplied."  -- error 1
                            _       -> trace (show taxas) $ Nothing
        noTaxaError =  if null taxas && not (foldr (\x acc -> acc || (areNewTaxa x)) False sequences) -- will register as False if sequences is empty
                                       then Just $ "Taxa are never specified. "  ++ (show taxas) ++ " " ++ (show sequences)-- error 6
                                       else Nothing
        seqTaxaCountErrors = foldr (\x acc -> (checkForNewTaxa x) : acc) [] sequences -- errors 7, 8 half of 16
        --correctCharCount = foldr (\x acc -> if isJust checkDims x
        --                                      then
        --                                      else ) Nothing convertSeqs ! 0
        symbols' = foldr (\x acc -> (getSymbols x) : acc) [] sequences
        taxaLst = (foldr (\x acc -> acc ++ getTaxaFromSeq x) [] sequences) ++
                            if length taxas > 0
                                then (taxaLabels $ head taxas)
                                else []
        -- taxaSeqVector = V.fromList [(taxon, alignedTaxaSeqMap M.! taxon) | taxon <- taxaLst]
        unalignedTaxaSeqMap = getSeqFromMatrix (getBlock "unaligned" sequences) taxaLst
        sequenceBlockErrors = case sequences of
                                 []        -> [Just "No characters or unalignd blocks provided."] -- error 3
                                 (x:y:z:_) -> [Just "Too many sequence blocks provided. Only one each of characters and unaligned blocks are allowed."] -- error 2
                                 (x:y:_)   -> if (aligned x) && (aligned y)
                                                then [Just "More than one characters block provided."] -- error 2
                                                else
                                                    if not ((aligned x) || (aligned y))
                                                        then [Just "More than one unaligned block provided."] -- error 3
                                                        else matrixDimsErrors  -- errors 4,5
                                 _         -> matrixDimsErrors  -- errors 4,5
        -- convertSeqs = ( concatSeqs . cleanSeqs sequences  -- convert each sequence, then

getEquates :: PhyloSequence -> Either String [String]
getEquates seq = eqs
    where
        form = headMay $ format seq
        eqs = if isJust form
              then equate $ fromJust form
              else Right [""]

-- TODO: This is too similar to getEquates, above. Can they be combined?
getSymbols :: PhyloSequence -> Either String [String]
getSymbols seq = eqs
    where
        form = headMay $ format seq
        eqs = if isJust form
              then symbols $ fromJust form
              else Right [""]

splitSequence :: Bool -> Bool -> String -> [[String]]
splitSequence isTokens isContinuous seq =
    if isTokens || isContinuous
        then findAmbiguousTokens (words seq) [] False
        else findAmbiguousNoTokens (strip seq) [] False

-- Parens and curly braces are treated the same
findAmbiguousNoTokens :: String -> [String] -> Bool -> [[String]]
findAmbiguousNoTokens [] _ _ = []
findAmbiguousNoTokens (x:xs) acc amb =
              case x of
                '{' -> findAmbiguousNoTokens xs [] True
                '}' -> acc : findAmbiguousNoTokens xs [] False
                '(' -> findAmbiguousNoTokens xs [] True
                ')' -> acc : findAmbiguousNoTokens xs [] False
                _   -> if amb
                           then findAmbiguousNoTokens xs (acc ++ [[x]]) amb
                           else [[x]] : findAmbiguousNoTokens xs [] amb

-- Maybe this and dimsMissing could be conflated.
matrixMissing :: PhyloSequence -> Maybe String
matrixMissing seq = if numMatrices  < 1
                    then tooFew
                    else if numMatrices > 1
                         then tooMany
                         else Nothing
                where
                    numMatrices = length (matrix seq)
                    tooFew = Just $ which ++ " block has no matrix."
                    tooMany = Just $ which ++ " block has more than one matrix."
                    which = if aligned seq
                                then "characters"
                                else "unaligned"

dimsMissing :: PhyloSequence -> Maybe String
dimsMissing seq = if numDims  < 1
                    then tooFew
                    else if numDims > 1
                         then tooMany
                         else Nothing
                where
                    numDims = length (matrix seq)
                    tooFew = Just $ which ++ " block has no dimensions."
                    tooMany = Just $ which ++ " block has more than one dimension."
                    which = if aligned seq
                                then "characters"
                                else "unaligned"


findAmbiguousTokens :: [String] -> [String] -> Bool -> [[String]]
findAmbiguousTokens [] _ _ = []
findAmbiguousTokens (x:xs) acc amb =
              if xHead == "{" || xHead == "("
                  then findAmbiguousTokens xs [xTail] True
                  else if xLast == "}" || xLast == ")"
                    then (acc ++ [takeWhile (\y -> y /= '}' && y /= ')') x]) : findAmbiguousTokens xs [] False
                    else if amb
                           then findAmbiguousTokens xs (acc ++ [x]) amb
                           else [x] : findAmbiguousTokens xs [] amb
              where
                xHead = safeHead x
                xTail = safeTail x
                xLast = safeLast x

safeLast :: [a] -> [a]
safeLast inLst =
    if length inLst > 0
        then [last inLst]
        else []

safeHead :: [a] -> [a]
safeHead inLst =
    if length inLst > 0
        then [head inLst]
        else []

safeTail :: [a] -> [a]
safeTail inLst =
    if length inLst > 1
        then tail inLst
        else []


findInterleaveError :: [String] -> PhyloSequence -> Maybe String
findInterleaveError [] _ = Nothing
findInterleaveError taxaLst seq =
    if interleaved && noLabels && (numLines `mod` numTaxa /= 0)
        then Just $ which ++ " block is not interleaved correctly. " ++ (show numLines) ++ " " ++ (show numTaxa)
        else Nothing
    where
        formatted = (length $ format seq) > 0
        interleaved = formatted && (interleave $ head $ format seq)
        noLabels = formatted && (unlabeled $ head $ format seq)
        numTaxa = length taxaLst
        numLines = length $ filter (\x -> (lstrip x) /= "") $ lines $ head $ matrix seq
        which = if aligned seq
                    then "Characters"
                    else "Unaligned"

getBlock :: String -> [PhyloSequence] -> [PhyloSequence]
getBlock _ [] = []
getBlock which seqs = if which == "aligned"
                          then f True seqs
                          else f False seqs
                      where
                          f xBool s = let block = headMay $ filter (\s -> xBool == (aligned s)) seqs
                                      in
                                          if isJust block
                                          then [fromJust block]
                                          else []


getSeqFromMatrix :: [PhyloSequence] -> [String] -> [M.Map String [[String]]]
getSeqFromMatrix [] _ = []
getSeqFromMatrix seqs taxaLst =
    if noLabels
        then if interleaved
            -- TODO: clean this ridiculous crap up.
            then [M.map (splitSequence tkns cont) 
                        (foldr 
                             (\x1 acc1 
                                 -> (foldr 
                                      (\x2 acc2 
                                           -> M.insert (fst x2) ((acc2 M.! (fst x2)) ++ (snd x2)) acc2)) 
                                      acc1 x1) 
                             (M.fromList (zip taxaLst [[] | x <- taxaLst])) (map (zip taxaLst) $ chunksOf numTaxa mtx))]
            else [M.fromList $ zip taxaLst (map (splitSequence tkns cont) mtx)]
        else trace (show seq') $ if interleaved
            then [M.map (splitSequence tkns cont) 
                        (foldr 
                            (\x acc 
                                 -> M.insert (fst x) ((acc M.! (fst x)) ++ (snd x)) acc)
                             (M.fromList (zip taxaLst [[] | x <- taxaLst])) 
                                             (map (\x -> let (name, seq) = span (/= ' ') x
                                                         in (name, seq)) mtx))]
            else [M.fromList $ map (\x -> let (name, seq) = span (/= ' ') x
                                          in (name, (splitSequence tkns cont seq))) mtx]
    where
        seq' = head seqs
        numTaxa = length taxaLst
        seqForm = headMay $ format seq'
        (noLabels, interleaved, tkns, cont) =
            if isJust seqForm
            then
                ( unlabeled $ fromJust seqForm
                , interleave $ fromJust seqForm
                , areTokens $ fromJust seqForm
                , map toLower (charDataType $ fromJust seqForm) == "continuous")
            else
                (False, False, False, False)
        mtx = filter (\x -> (lstrip x) /= "") $ lines $ head $ matrix seq' -- I've already checked to make sure there's a matrix


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = f : chunksOf n s
  where
    (f,s) = splitAt n xs

areNewTaxa :: PhyloSequence -> Bool
areNewTaxa seq = case (charDims seq) of
                    [] -> False
                    xs  -> case (seqTaxaLabels seq) of
                            [] -> False
                            _  -> newTaxa $ head xs

--checkDims :: Phylosequence -> Maybe String
--checkDims seq = let dim = numchars $ head (charDims seq)
--                in if dim == length

-- This is frighteningly similar to areNewTaxa, but I couldn't figure out a way around
-- having them both, because of the way that areNewTaxa is used
checkForNewTaxa :: PhyloSequence -> Maybe String
checkForNewTaxa seq = case (charDims seq) of
                        [] -> Nothing
                        xs  -> if newTaxa $ head xs
                              then
                                  case (seqTaxaLabels seq) of
                                      [] -> Just $ "In the " ++ which ++ " block the newtaxa keyword is specified, but no new taxa are given."
                                      ys  -> if (length $ head ys) == (numTaxa $ head xs)
                                             then Just $ "In the " ++ which ++ " block the number of new taxa does not match the number of taxa specified."
                                             else Nothing
                              else Nothing
                    where
                        which = if aligned seq
                                then "characters"
                                else "unaligned"

getTaxaFromSeq :: PhyloSequence -> [String]
getTaxaFromSeq seq = if areNewTaxa seq
                        then head $ seqTaxaLabels seq
                        else []

convertMatrix :: [String] -> PhyloSequence -> V.Vector (String, [String])
convertMatrix taxa sequence = V.fromList [("dummy", ["data", "here"])]

-- | Checks for aligned block. If block should be aligned, but isn't, returns error String.
-- Otherwise, Nothing (no errors).
-- Also makes sure given dimensions match length of sequence found
--isAligned :: V.Vector (String, [String]) -> Maybe String
--isAligned seqs = case
--    V.foldr (\x acc -> if ) Nothing seqs
--                 where initLength = length $ snd (seqs ! 0)

parseNexus :: Parsec String u ParseResult
parseNexus = nexusFileDefinition

nexusFileDefinition :: Parsec String u ParseResult
nexusFileDefinition = do
    _       <- string "#NEXUS"
    _       <- spaces
    comment <- optionMaybe commentDefinition
    _       <- spaces
    (x,y,z) <- partitionNexusBlocks <$> (many nexusBlock)
    pure $ ParseResult x y z

ignoredBlockDefinition :: Parsec String u String
ignoredBlockDefinition = do
    title <- many letter
    _     <- symbol (caseInsensitiveString ";")
    _     <- whitespace
    _     <- anyTill $ symbol (caseInsensitiveString "END;")
    pure $ title

nexusBlock :: Parsec String u NexusBlock
nexusBlock = do
        _      <- symbol (caseInsensitiveString "BEGIN")
        block' <- symbol block
        _      <- symbol (caseInsensitiveString "END;")
        pure $ block'
    where
        block =  (CharacterBlock <$> try (characterBlockDefinition "characters" True))
             <|> (CharacterBlock <$> try (characterBlockDefinition "unaligned" False))
             <|> (CharacterBlock <$> try (characterBlockDefinition "data" True)) -- data blocks should be aligned
             <|> (TaxaBlock      <$> try taxaBlockDefinition)
             <|> (TreesBlock     <$> try treeBlockDefinition)
             <|> (IgnoredBlock   <$> try ignoredBlockDefinition)

characterBlockDefinition :: String -> Bool -> Parsec String u PhyloSequence
characterBlockDefinition which aligned = do
    _           <- symbol (caseInsensitiveString $ which ++ ";")
    (v,w,x,y,z) <- partitionSequenceBlock <$> (many seqSubBlock)
    pure $ PhyloSequence aligned v w x y z

taxaBlockDefinition :: Parsec String u TaxaSpecification
taxaBlockDefinition = do
    _     <- symbol (caseInsensitiveString "taxa;")
    (y,z) <- partitionTaxaBlock <$> (many seqSubBlock)
    pure $ TaxaSpecification y z

taxaSubBlock :: Parsec String u SeqSubBlock
taxaSubBlock = do
        _      <- optionMaybe whitespace
        block' <- symbol block
        pure block'
    where
        block =  (Dims <$> try dimensionsDefinition)
             <|> (Taxa <$> try (stringListDefinition "taxlabels"))
             <|> (Ignored <$> try (ignoredSubBlockDef ';'))

treeBlockDefinition :: Parsec String u TreeBlock
treeBlockDefinition = do
        _     <- symbol (caseInsensitiveString "trees;")
        (x,y) <- partitionTreeBlock <$> (many treeFieldDef)
        pure $ TreeBlock x y

seqSubBlock :: Parsec String u SeqSubBlock
seqSubBlock = do
        _      <- optionMaybe whitespace
        block' <- symbol block
        pure block'
    where
        block =  (Dims <$> try dimensionsDefinition)
             <|> (Format <$> try formatDefinition)
             <|> (Eliminate <$> try (stringDefinition "eliminate"))
             <|> (Matrix <$> try matrixDefinition)
             <|> (Taxa <$> try (stringListDefinition "taxlabels"))
             <|> (Ignored <$> try (ignoredSubBlockDef ';'))

dimensionsDefinition :: Parsec String u DimensionsFormat
dimensionsDefinition = do
        _         <- symbol (caseInsensitiveString "dimensions")
        newTaxa'  <- optionMaybe (try (symbol (caseInsensitiveString "newTaxa")))
        _         <- optionMaybe (try (symbol (caseInsensitiveString "nTax")))
        _         <- optionMaybe $ try (symbol (char '='))
        numTaxa'  <- optionMaybe $ try (symbol integer)
        _         <- optionMaybe $ symbol (caseInsensitiveString "nchar")
        _         <- optionMaybe $ symbol (char '=')
        charCount <- optionMaybe $ try (symbol integer)
        _         <- symbol $ char ';'
        pure $ DimensionsFormat (newTaxa' /= Nothing)
                                (if numTaxa' == Nothing then 0 else read (fromJust numTaxa') :: Int)
                                (if charCount == Nothing then 0 else read (fromJust charCount) :: Int)
    where
        plus    = char '+' *> number
        minus   = char '-' <:> number
        number  = many1 digit
        integer = plus <|> minus <|> number

formatDefinition :: Parsec String u CharacterFormat
formatDefinition = do
        _                         <- symbol (caseInsensitiveString "format")
        (o,p,q,r,s,t,u,v,w,x,y,z) <- partitionCharFormat <$> charFormatFieldDef
        _                         <- symbol $ char ';'
        pure $ CharacterFormat o p q r s t u v w x y z

charFormatFieldDef :: Parsec String u [CharFormatField]
charFormatFieldDef = do
        block' <- many block
        pure block'
    where
        block =  (CharDT <$> try (stringDefinition "datatype"))
             <|> (SymStr <$> try (quotedStringDefinition "symbols"))
             <|> (Transpose <$> try (booleanDefinition "transpose"))
             <|> (Interleave <$> try (booleanDefinition "interleave"))
             <|> (Tokens <$> try (booleanDefinition "tokens"))
             <|> (EqStr <$> try (quotedStringDefinition "equate"))
             <|> (MissStr <$> try (stringDefinition "missing"))
             <|> (GapChar <$> try (stringDefinition "gap"))
             <|> (MatchChar <$> try (stringDefinition "matchcar"))
             <|> (Items <$> try (stringDefinition "items"))
             <|> (RespectCase <$> try (booleanDefinition "respectcase"))
             <|> (Unlabeled <$> try (booleanDefinition "nolabels"))
             <|> (IgnFF <$> try (ignoredSubBlockDef ' '))

treeFieldDef :: Parsec String u TreeField
treeFieldDef = do
        block' <- block
        pure block'
    where
        block =  (Translation <$> try (delimitedStringListDefinition "translate" ','))
             <|> (Tree <$> try treeDefinition)
             <|> (IgnTF <$> try (ignoredSubBlockDef ';'))



booleanDefinition :: String -> Parsec String u Bool
booleanDefinition blockTitle = do
    title <- symbol (caseInsensitiveString blockTitle)
    -- _     <- symbol $ char ';'
    pure $ ((map toUpper title) == (map toUpper blockTitle))

stringDefinition :: String -> Parsec String u String
stringDefinition blockTitle = do
    _     <- symbol (caseInsensitiveString blockTitle)
    _     <- symbol $ char '='
    value <- symbol $ notKeywordWord ""
    -- _     <- symbol $ char ';'
    pure $ value

-- TODO?: This doesn't work if they leave off the opening quote mark.
quotedStringDefinition :: String -> Parsec String u (Either String [String])
quotedStringDefinition blockTitle = do
    _     <- symbol (caseInsensitiveString blockTitle)
    _     <- symbol $ char '='
    _     <- symbol $ char '"'
    value <- many1 $ symbol $ notKeywordWord "\""
    close <- optionMaybe $ char '"'
    -- _     <- symbol $ char ';'
    pure $ if isJust close 
           then Right value 
           else Left (blockTitle ++ " missing closing quote.")

stringListDefinition :: String -> Parsec String u [String]
stringListDefinition label = do
        _        <- symbol (caseInsensitiveString label)
        theItems <- many $ symbol $ notKeywordWord ""
        _        <- symbol $ char ';'
        pure $ theItems

delimitedStringListDefinition :: String -> Char -> Parsec String u [String]
delimitedStringListDefinition label delimiter = do
    _        <- symbol (caseInsensitiveString label)
    theItems <- many (noneOf $ delimiter : ";") `sepBy` (char delimiter)
    _        <- symbol $ char ';'
    pure $ theItems


treeDefinition :: Parsec String u (String, String)
treeDefinition = do
    _     <- symbol (caseInsensitiveString "tree")
    label <- symbol $ many (noneOf ";=")
    _     <- symbol $ char '='
    trees <- symbol $ many (noneOf ";")
    _     <- symbol $ char ';'
    pure (label, trees)

matrixDefinition :: Parsec String u String
matrixDefinition = do
    first     <- symbol (caseInsensitiveString "matrix")
    goodStuff <- many $ noneOf ";"
    _         <- symbol $ char ';'
    pure goodStuff

ignoredSubBlockDef :: Char -> Parsec String u String
ignoredSubBlockDef endChar = do
    title <- anyTill (symbol (caseInsensitiveString "end;")
                      <|> symbol (caseInsensitiveString ";")
                      <|> symbol (caseInsensitiveString [endChar])
                     )
    _     <- symbol $ char endChar -- didn't think I needed this,
                                   -- but otherwise I get
                                   -- "many applied to parser that accepts empty string"
    pure title

-- -------------------------------------------------------------------------------------------------
-- | Partitioning functions, which take a list of some type and produce a tuple.
-- Where there is a block with multiple optional fields or a field with multiple optional
-- subfields these take the output and put it into a tuple which can then be decomposed
-- and its fields used as arguments to a constructor.
-- I'm wondering if there's isn't a more efficient way to do this.
-- Also, can these be reduced to a single function, since they're all doing the same thing?

partitionSequenceBlock :: [SeqSubBlock] -> ([String],[CharacterFormat],[DimensionsFormat],[String],[[String]])
partitionSequenceBlock = foldr f ([],[],[],[],[])
    where
        f (Matrix n)     (v,w,x,y,z) = (n:v,   w,   x,   y,   z)
        f (Format n)     (v,w,x,y,z) = (  v, n:w,   x,   y,   z)
        f (Dims n)       (v,w,x,y,z) = (  v,   w, n:x,   y,   z)
        f (Eliminate n)  (v,w,x,y,z) = (  v,   w,   x, n:y,   z)
        f (Taxa n)       (v,w,x,y,z) = (  v,   w,   x,   y, n:z)
        f (Ignored n)             ws = ws

partitionTaxaBlock :: [SeqSubBlock] -> (Int, [String])
partitionTaxaBlock = foldr f (0,[])
    where
        f (Dims n) (y,z) = (num, z)
            where
                num = numTaxa n
        f (Taxa n) (y,z) = (  y, n)
        f _           ws = ws
            



partitionNexusBlocks :: [NexusBlock] -> ([PhyloSequence], [TaxaSpecification], [TreeBlock])
partitionNexusBlocks = foldr f ([],[],[])
  where
    f (CharacterBlock n) (xs,ys,zs) = (n:xs,   ys,   zs)
    f (TaxaBlock n)      (xs,ys,zs) = (  xs, n:ys,   zs)
    f (TreesBlock n)     (xs,ys,zs) = (  xs,   ys, n:zs)
    f _                          ws = ws

partitionCharFormat :: [CharFormatField] -> (String, Either String [String], Either String [String], String, String, String, String, Bool, Bool, Bool, Bool, Bool)
partitionCharFormat = foldr f ("", Right [""], Right [""], "", "", "", "", False, False, False, False, False)
    where
        f (CharDT n)      (p,q,r,s,t,u,v,w,x,y,z,o) = (n,q,r,s,t,u,v,w,x,y,z,o)
        f (SymStr n)      (p,q,r,s,t,u,v,w,x,y,z,o) = (p,n,r,s,t,u,v,w,x,y,z,o)
        f (EqStr n)       (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,n,s,t,u,v,w,x,y,z,o)
        f (MissStr n)     (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,n,t,u,v,w,x,y,z,o)
        f (GapChar n)     (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,n,u,v,w,x,y,z,o)
        f (MatchChar n)   (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,n,v,w,x,y,z,o)
        f (Items n)       (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,n,w,x,y,z,o)
        f (RespectCase n) (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,v,n,x,y,z,o)
        f (Tokens n)      (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,v,w,n,y,z,o)
        f (Transpose n)   (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,v,w,x,n,z,o)
        f (Interleave n)  (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,v,w,x,y,n,o)
        f (Unlabeled n)   (p,q,r,s,t,u,v,w,x,y,z,o) = (p,q,r,s,t,u,v,w,x,y,z,n)
        f (IgnFF n)                              ws = ws

partitionTreeBlock :: [TreeField] -> ([[String]], [(String,String)])
partitionTreeBlock = foldr f ([],[])
    where
        f (Translation n) (ys,zs) = (n:ys,   zs)
        f (Tree n)        (ys,zs) = (  ys, n:zs)
        f _                    ws = ws


trimmed :: Parsec String u a -> Parsec String u a
trimmed x = whitespace *> x <* whitespace

symbol :: Parsec String u a -> Parsec String u a
symbol x = x <* whitespace

whitespace :: Parsec String u ()
whitespace = (many1 (commentDefinition) *> pure ())
          <|> spaces
          <?> "white space"

commentDefinition :: Parsec String u String
commentDefinition = commentDefinition' False
    where
        commentContent = many (noneOf "[]")
        commentDefinition' enquote = do
            _        <- char '[' <?> "\"[\" to begin a comment definition"
            before   <- commentContent
            comments <- many (commentDefinition' True <++> commentContent)
            _        <- char ']' <?> "\"]\" to correctly close comment"
            _        <- spaces
            pure . concat $
                if enquote
                then "[" : before : comments ++ ["]"]
                else       before : comments


spaces1 :: Parsec String u ()
spaces1 = space *> spaces

lstrip :: String -> String
lstrip "" = ""
lstrip input = dropWhile (\x -> x `elem` " \t\n\r") input

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

notKeywordWord :: (Monad m) => String -> ParsecT String u m String
notKeywordWord avoidChars = do
    word <- lookAhead $ nextWord
    if (toLower <$> word) `S.member` keywords
    then fail $ "Unexpected keyword '" ++ word ++ "', perhaps you are missing a semicolon?"
    else nextWord
  where
    nextWord = many1 $ try $ satisfy (\x -> (not $ elem x (';' : avoidChars)) && (not $ isSpace x))
    keywords = S.fromList ["ancstates", "assumptions", "begin", "changeset", "characters", "charlabels", "charpartition", "charset", "charstatelabels", "codeorder", "codeset", "codons", "data", "datatype", "deftype", "diagonal", "dimensions", "distances", "eliminate", "end", "equate", "exset", "extensions", "format", "gap", "interleave", "items", "labels", "matchchar", "matrix", "missing", "nchar", "newtaxa", "nodiagonal", "nolabels", "notes", "notokens", "ntax", "options", "picture", "respectcase", "sets", "statelabels", "statesformat", "symbols", "taxa", "taxlabels", "taxpartition", "taxset", "text", "tokens", "translate", "transpose", "tree", "treepartition", "trees", "treeset", "triangle", "typeset", "unaligned", "usertype", "wtset"]

