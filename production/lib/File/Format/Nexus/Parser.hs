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
-- Functions for for parsing and validating Nexus files.
--
-- Will function correctly if and only if we are in a state of harmonic convergence.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse, FlexibleContexts #-}

-- TODOs:
-- • Figure out why PROTPARS-example.nex won't parse
-- • Step matrices
-- • check for and eliminate thusly noted characters (in at least two places?)
-- • Split verification & parsing into two modules, maybe three (one for data types)
-- • update output datatypes: nest vectors, add character metadata
-- • Verify character metadata, especially alphabets
-- • Reconceive and reorder current validations
-- • Fix three broken test cases

module File.Format.Nexus.Parser where

import Data.Char   (isSpace,toLower)
import Data.Either (lefts)
import Data.List   (sort)
import qualified Data.Map.Lazy as M
--import Data.Matrix            (Matrix)
import Data.Maybe  (isJust, fromJust, catMaybes, maybeToList)
import qualified Data.Set as S
import Debug.Trace -- <-- the best module!!! :)
import File.Format.Newick
import File.Format.TransitionCostMatrix.Parser
import Safe
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Lexer  (integer)
import Text.Megaparsec.Prim   (MonadParsec)
import Text.Megaparsec.Custom
import qualified Data.Vector as V

data NexusParseResult = NexusParseResult [PhyloSequence] [TaxaSpecification] [TreeBlock] [AssumptionBlock] [IgnBlock] deriving (Show)

-- | Types blocks in the Nexus file and their accompanying data.
data NexusBlock
   = TaxaBlock        TaxaSpecification
   | CharacterBlock   PhyloSequence
   | TreesBlock       TreeBlock
   | SkippedBlock     IgnBlock
   | AssumptionsBlock AssumptionBlock
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
   , matrix        :: [[String]]
   , format        :: [CharacterFormat]
   , charDims      :: [DimensionsFormat] -- holds length of sequence, as well as info on new taxa
   , elims         :: [String]
   , seqTaxaLabels :: [[String]]
   , name          :: String -- characters, taxa or data
   } deriving (Show)

-- | SeqSubBlock is list of fields available in sequence blocks. It's set up as an enumeration
-- so that it can be "looped" over when parsing the sequence blocks, as the order of the fields
-- is not documented.
data SeqSubBlock
   = Matrix          [String]
   | Format          CharacterFormat
   | Dims            DimensionsFormat
   -- | Items           ItemType
   | Eliminate       String
   | CharStateLabels [CharStateFormat]
   | IgnSSB          String
   | Taxa            [String]
   deriving (Show)

data TaxaSpecification
   = TaxaSpecification
   { taxaDims   :: Int
   , taxaLabels :: [String]
   } deriving (Show)

data IgnBlock = IgnBlock {ignoredName :: String} deriving (Show)

-- | The different subfields of the Format field in the sequence blocks.
-- As with SeqSubBlock, listed simply so that it can be "looped" over. Will eventually be
-- coverted to CharacterFormat data type for export
-- TODO: better documentation on the use of each field below
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
   deriving (Eq,Show)

type TreeName       = String
type SerializedTree = String

data AssumptionBlock
   = AssumptionBlock
   { tcm :: [StepMatrix] } deriving (Show)

data AssumptionField
   = TCMMat StepMatrix
   | IgnAF  String

data StepMatrix
   = StepMatrix
   { matrixType :: String
   , matrixSize :: Int
   , matrixData :: TCM
   } deriving (Show)

data TreeBlock
   = TreeBlock
   { translate :: [[String]]
   , trees     :: [(TreeName, [NewickForest])]
   } deriving (Show)

data TreeField
   = Translation [String]
   | Tree        (TreeName, [NewickForest])
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
   , symbols      :: Either String [String]
   , equate       :: Either String [String]
   , missing      :: String
   , gap          :: String
   , matchChar    :: String
   , items        :: String
   , respectCase  :: Bool
   , areTokens    :: Bool
   , transpose    :: Bool
   , interleave   :: Bool
   , unlabeled    :: Bool
   } deriving (Eq,Show)

-- | The types of data which can be present in a Nexus file.
-- This type might get scrapped and another type imported from
-- different module, or preserved but moved to another module.
data CharDataType = Standard | DNA | RNA | Nucleotide | Protein | Continuous deriving (Read, Show)

-- | The collection of information extracted from blocks in the Nexus file.
data Nexus
   = Nexus
   -- TODO: taxa was commented out before first push to Grace
   { {- taxa :: [String]
   ,-} characters :: M.Map String (V.Vector [String])
   --, characters :: SequenceBlock
   } deriving (Read,Show)

data SequenceBlock
   = SequenceBlock
   { charType  :: CharDataType
   , alphabet  :: String
   , isAligned :: Bool
   , seqs      :: [V.Vector (String, [String])]
   } deriving (Show)

parseNexusStream :: String -> Either ParseError Nexus
parseNexusStream = parse (validateNexusParseResult =<< parseNexus <* eof) "PCG encountered a Nexus file parsing error it could not overcome:"

-- TODO: replace equate chars, ignore case, check alignment and length of aligned blocks, check alphabet, think about 12 and 20, below, make each sequence a vector, not a list


-- Because some errors are dependent on passing some other validation, they've been split into
-- dependant and independant errors. Errors are enumerated below, along with their dependancies.
-- Comments are included in the code that explain which of the below errors are caught at various points.
-- Note that nos. 4--6, below, are only dependent on 3, so are simply done as nested ifs. This may
-- or may not be best practice.                                                             coded as
-- Errors currently caught: # error                                       dependency   inde- or de- pendant    finished
--  1. Too many taxa blocks                                                    -              indep              done
--  ** removed ** too many sequence blocks                                                -              indep              done
--     There are actually two subcases here:
--        too many aligned, too many unaligned
--  ** removed ** No sequence blocks                                                      -              indep              done
--  4. No matrix in some sequence block                                        3              indep              done
--  5. No dimensions in some sequence block                                    3              indep              done
--  6. Sequence block with "nolabels", but no taxa block _and_ no new taxa specified in sequence blocks            3              indep              done
--  7. "newtaxa" keyword in characters or unaligned block, but no taxa actually spec'ed            3                dep              done
--  8. "newtaxa" keywork in characters or unaligned block, but ntaxa missing                       3,5              dep              done
--  9. "nolabels" but labels actually present--subsumed under 10, 11, 12       3                dep
-- 10. Matrix has alphabet values not specified                                3,4              dep
-- 11. Aligned block not Aligned                                               3,4              dep
-- 12. Matrix has non-spec'ed taxa                                             1,3,4,5          dep              done
-- 13. Matrix interleaved, but some blocks missing taxa                        1,3,4,5          dep              done
--     ---for aligned matrices, caught by 10
--     ---for unaligned, caught by combo of 12 & 22
-- 14. Matrix interleaved, unlabeled, but length not a multiple of # of taxa   1,3,4            dep              done
-- 15. Character count is incorrect                                            3,4,5            dep
-- 16. Taxa count is incorrect                                                 1,3,5            dep              done
-- 17. "nolabels" but there is a taxa block and newtaxa in seq block,          1,3,7,8          dep
--     so order of sequences is unclear
-- 18. Missing semicolons                                                       -             indep              caught in parse
-- 19. Equate or symbols strings missing their closing quotes                  3                dep              done
-- 20. Match character can't be a space                                        3                dep
--     ---for aligned blocks, will be caught
--     ---as with 13, not sure how to catch for unaligned
-- 21. Space in taxon name                                                     5,6              dep              done
--     ---for aligned, should be caught by 16
--     ---for unaligned, caught by combo of 12 & 22
-- 22. In unaligned, interleaved block, a taxon is repeated                    3                dep              done
validateNexusParseResult :: (Show s, MonadParsec s m Char) => NexusParseResult -> m Nexus
validateNexusParseResult (NexusParseResult sequences taxas _treeSet _assumptions _ignored)
  | not (null independentErrors) = fails independentErrors
  | not (null dependentErrors)   = fails dependentErrors
  -- TODO: first arg to Nexus was commented out before first push to Grace
  -- TODO: unalignedTaxaSeqMap was commented out before first push to Grace.
  -- When it's added back, downstream (i.e. Nexus) fns will need to be modified
  -- to expect a *list* of 
  | otherwise                  = pure $ Nexus {-taxaLst-} (alignedTaxaSeqMap {- : unalignedTaxaSeqMap -})
  where
        alignedTaxaSeqMap = getSeqFromMatrix (getBlock "aligned" sequences) taxaLst
        --TODO: dependentErrors & independentErrors becomes :: String error, String warning => [Maybe (Either error warning)]
        -- then partitionEithers . catMaybes, etc., etc.
        dependentErrors = catMaybes $ incorrectTaxaCount : (missingCloseQuotes ++ seqTaxaCountErrors ++ interleaveErrors ++ seqTaxonCountErrors ++ incorrectCharCount)
        equates = foldr (\x acc -> getEquates x : acc) [] sequences
        independentErrors = catMaybes $ noTaxaError : multipleTaxaBlocks : sequenceBlockErrors
        f [x] = Just x
        f _   = Nothing

        incorrectCharCount  = checkSeqLength (getBlock "aligned" sequences) alignedTaxaSeqMap
        seqTaxonCountErrors = foldr (\x acc -> getSeqTaxonCountErrors taxaLst x ++ acc) [] sequences -- errors 12, 22
        incorrectTaxaCount  = f taxas >>= \(TaxaSpecification num taxons) -> if num /= length taxons
                then Just $ "Incorrect number of taxa in taxa block.\n" {- ++ (show num) ++ " " ++ (show taxons) -} -- half of error 16
                else Nothing
        interleaveErrors = foldr (\x acc -> findInterleaveError taxaLst x : acc) [] sequences -- error 14
        matrixDimsErrors = foldr (\x acc -> matrixMissing x : acc) [] sequences ++
                           foldr (\x acc -> dimsMissing   x : acc) [] sequences
        missingCloseQuotes = map Just (lefts equates) ++ map Just (lefts symbols') -- error 19
        multipleTaxaBlocks = case taxas of
                            (_:_:_) -> Just "Multiple taxa blocks supplied.\n"  -- error 1
                            _       -> Nothing
        noTaxaError =  if null taxas && not (foldr (\x acc -> acc || areNewTaxa x) False sequences) -- will register as False if sequences is empty
                       then Just $ "Taxa are never specified. \n"  {-++ (show taxas) ++ " " ++ (show sequences) -} -- error 6
                       else Nothing
        seqTaxaCountErrors = foldr (\x acc -> checkForNewTaxa x : acc) [] sequences -- errors 7, 8 half of 16
        --correctCharCount = foldr (\x acc -> if isJust checkDims x
        --                                      then
        --                                      else ) Nothing convertSeqs ! 0
        symbols' =  foldr (\x acc -> getSymbols x : acc) [] sequences
        taxaLst  = foldr (\x acc -> acc ++ getTaxaFromSeq x) [] sequences ++
                            if   (not . null) taxas
                            then taxaLabels $ head taxas
                            else []
        -- taxaSeqVector = V.fromList [(taxon, alignedTaxaSeqMap M.! taxon) | taxon <- taxaLst]
        --unalignedTaxaSeqMap = getSeqFromMatrix (getBlock "unaligned" sequences) taxaLst
        sequenceBlockErrors = case sequences of
                                 []        -> [Just "No characters or unaligned blocks provided.\n"] -- error 3
                                 (_:_:_:_) -> [Just "Too many sequence blocks provided. Only one each of characters and unaligned blocks are allowed.\n"] -- error 2
                                 (x:y:_) | aligned x && aligned y       -> [Just "More than one characters block provided."]
                                         | not (aligned x || aligned y) -> [Just "More than one unaligned block provided.\n"]
                                         | otherwise                    -> matrixDimsErrors
                                 _         -> matrixDimsErrors
        --taxaFromSeqMatrix = foldr (\x acc -> (getTaxaFromMatrix x) ++ acc) [] sequences
        -- convertSeqs = ( concatSeqs . cleanSeqs sequences  -- convert each sequence, then

checkSeqLength :: [PhyloSequence] -> M.Map String (V.Vector [String]) -> [Maybe String]
checkSeqLength [] _ = [Nothing]
checkSeqLength seq' seqMap =
    M.foldrWithKey (\key val acc -> (if length val == len
                                     then Nothing
                                     else Just (key ++ "'s sequence is the wrong length in an aligned block. It should be " ++ show len ++ ", but is " ++ show (length val) {- ++ ":\n" ++ show val -} ++ "\n")) : acc) [] seqMap
    where
        len = numChars . head . charDims $ head seq'


getSeqTaxonCountErrors :: [String] -> PhyloSequence -> [Maybe String]
getSeqTaxonCountErrors taxaLst seq' = extraTaxonErrors ++ wrongCountErrors
    where
        seqTaxaMap = getTaxaFromMatrix seq'
        listedTaxaMap = M.fromList $ zip taxaLst ([1..] :: [Int])
        extraTaxonErrors = M.foldrWithKey
                                (\key _ acc -> (if M.member key listedTaxaMap
                                                then Nothing
                                                else Just ("\"" ++ key ++ "\" is in a matrix, but isn't specified anywhere, such as in a taxa block or as newtaxa.\n"))
                                                  : acc
                                 ) [] seqTaxaMap
        wrongCountErrors = M.foldrWithKey (\key val acc -> (if val /= median
                                                                   then Just ("\"" ++ key ++ "\" appears the wrong number of times in a matrix.\n")
                                                                   else Nothing) : acc
                                          ) [] seqTaxaMap
        median = findMedian $ M.elems seqTaxaMap

findMedian :: Ord a => [a] -> a
findMedian xs = sort xs !! quot (length xs) 2


getEquates :: PhyloSequence -> Either String [String]
getEquates = maybe (Right [""]) equate . headMay . format

getTaxaFromMatrix :: PhyloSequence -> M.Map String Int
getTaxaFromMatrix seq' = {-trace (show taxa) $ -}
    if noLabels
        then M.empty
        else taxaMap
    where
        (noLabels, _interleaved, _tkns, _cont, _matchChar') = getFormatInfo seq'
        mtx     = head $ matrix seq' -- I've already checked to make sure there's a matrix
        taxaMap = foldr (\x acc -> M.insert x (succ (M.findWithDefault 0 x acc)) acc) M.empty taxa
        taxa    = foldr (\x acc -> takeWhile (`notElem` " \t") x : acc) [] mtx

-- TODO: This is too similar to getEquates, above. Can they be combined?
getSymbols :: PhyloSequence -> Either String [String]
getSymbols = maybe (Right [""]) symbols . headMay . format

splitSequence :: Bool -> Bool -> String -> V.Vector [String]
splitSequence isTokens isContinuous seq' = V.fromList $
    if isTokens || isContinuous
        then findAmbiguousTokens (words seq') [] False
        else findAmbiguousNoTokens (strip seq') [] False

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
matrixMissing seq'
  | numMatrices < 1 = tooFew
  | numMatrices > 1 = tooMany
  | otherwise       = Nothing
  where
    numMatrices = length $ matrix seq'
    tooFew      = Just $ name seq' ++ " block has no matrix.\n"
    tooMany     = Just $ name seq' ++ " block has more than one matrix.\n"

dimsMissing :: PhyloSequence -> Maybe String
dimsMissing seq'
  | numDims < 1 = tooFew
  | numDims > 1 = tooMany
  | otherwise   = Nothing
  where
    numDims = length $ charDims seq'
    tooFew  = Just $ name seq' ++ " block has no dimensions.\n"
    tooMany = Just $ name seq' ++ " block has more than one dimension.\n"

findAmbiguousTokens :: [String] -> [String] -> Bool -> [[String]]
findAmbiguousTokens [] _ _ = []
findAmbiguousTokens (x:xs) acc amb
  | xHead == "{" || xHead == "(" = findAmbiguousTokens xs [xTail] True
  | xLast == "}" || xLast == ")" = (acc ++ [takeWhile (\ y -> y /= '}' && y /= ')') x]) : findAmbiguousTokens xs [] False
  | amb                          = findAmbiguousTokens xs (acc ++ [x]) amb
  | otherwise                    = [x] : findAmbiguousTokens xs [] amb
  where
    xHead = safeHead x
    xTail = safeTail x
    xLast = safeLast x

safeLast :: [a] -> [a]
safeLast inLst = [ last inLst | not (null inLst) ]

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:_) = [x]

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

findInterleaveError :: [String] -> PhyloSequence -> Maybe String
findInterleaveError [] _ = Nothing
findInterleaveError taxaLst seq' =
    if interleaved && noLabels && (lineCount `mod` taxaCount /= 0)
        then Just $ which ++ " block is not interleaved correctly. \n" {- ++ (show numLines) ++ " " ++ (show numTaxa) -}
        else Nothing
    where
        formatted   = not . null $ format seq'
        interleaved = formatted && interleave (head $ format seq')
        noLabels    = formatted && unlabeled  (head $ format seq')
        taxaCount   = length taxaLst
        lineCount   = length . head $ matrix seq'
        which = if aligned seq'
                    then "Characters"
                    else "Unaligned"

-- TODO: Review this function's functionality. Seems like it can be improved!
getBlock :: String -> [PhyloSequence] -> [PhyloSequence]
getBlock _ [] = []
getBlock which pseqs = f (which == "aligned") pseqs
  where
    f xBool _ = let block = headMay $ filter (\s -> xBool == aligned s) pseqs
                in maybeToList block


getFormatInfo :: PhyloSequence -> (Bool, Bool, Bool, Bool, String)
getFormatInfo pseq = case headMay $ format pseq of
                       Nothing -> (False, False, False, False, "")
                       Just x  -> ( unlabeled x
                                  , interleave x
                                  , areTokens x
                                  , map toLower (charDataType x) == "continuous"
                                  , matchChar x
                                  )

getSeqFromMatrix :: [PhyloSequence] -> [String] -> M.Map String (V.Vector [String])
getSeqFromMatrix [] _ = mempty
getSeqFromMatrix seqLst taxaLst =
    M.map (splitSequence tkns cont) matchCharsReplaced
    where
        (noLabels, interleaved, tkns, cont, matchChar') = getFormatInfo $ head seqLst
        taxaCount  = length taxaLst
        taxaMap    = M.fromList . zip taxaLst $ repeat []
        mtx        = head $ matrix $ head seqLst -- I've already checked to make sure there's a matrix
        entireSeqs = if noLabels    -- this will be a list of tuples (taxon, concatted seq)
                     then if interleaved
                          then concatMap (zip taxaLst) (chunksOf taxaCount mtx)
                          else zip taxaLst mtx
                     else getTaxonAndSeqFromMatrixRow <$> mtx
        entireDeinterleavedSeqs = if interleaved
                                  then deInterleave taxaMap entireSeqs -- next step: figure out why entireSeqs is sometimes not split
                                  else M.fromList entireSeqs
        firstSeq = fromJust $ M.lookup (if noLabels
                                        then head taxaLst
                                        else takeWhile (`notElem` " \t") $ head mtx)
                            entireDeinterleavedSeqs
        matchCharsReplaced = if matchChar' /= ""
                             then M.map (replaceMatches (head matchChar') firstSeq) entireDeinterleavedSeqs
                             else entireDeinterleavedSeqs

-- | deInterleaved takes in an interleaved matrix in the form [(taxon,sequence)] and returns
-- a matrix of the form Map taxon sequence. The original list of tuples should have duplicate taxon entries
-- (because of the matrix being interleaved), and the seqs should be concatted---in order---in the 
-- returned map.
-- A (partial?) test exists in the test suite.
deInterleave :: M.Map String String -> [(String, String)] -> M.Map String String
deInterleave = foldr (\(seqName, pseq) acc -> M.insertWith (++) seqName pseq acc)

-- | getTaxonAndSeqFromMatrixRow takes a String of format "xxx[space or tab]yyy"
-- and returns a tuple of form ("xxx","yyy")
-- A test exists in the test suite.
getTaxonAndSeqFromMatrixRow :: String -> (String, String)
getTaxonAndSeqFromMatrixRow inStr = (seqName, pseq)
    where 
        (seqName, rest) = span   (`notElem` " \t") inStr
        pseq            = dropWhile (`elem` " \t") rest

replaceMatches :: Char -> String -> String -> String
replaceMatches matchTarget canonical toReplace = {- trace (canonical ++"\n" ++ toReplace ++ "\n") $ -}
    zipWith f canonical toReplace
    where
        f x y = if y == matchTarget
                then x
                else y

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = f : chunksOf n s
  where
    (f,s) = splitAt n xs

areNewTaxa :: PhyloSequence -> Bool
areNewTaxa pseq
    | name pseq == "data" = True
    | otherwise          =
      case charDims pseq of
        []  -> False
        xs  -> case seqTaxaLabels pseq of
                [] -> False
                _  -> newTaxa $ head xs

--checkDims :: Phylosequence -> Maybe String
--checkDims seq = let dim = numchars $ head (charDims seq)
--                in if dim == length

-- This is frighteningly similar to areNewTaxa, but I couldn't figure out a way around
-- having them both, because of the way that areNewTaxa is used
checkForNewTaxa :: PhyloSequence -> Maybe String
checkForNewTaxa pseq = case charDims pseq of
                         [] -> Nothing
                         xs -> if newTaxa $ head xs
                               then
                                 case seqTaxaLabels pseq of
                                  [] -> Just $ "In the " ++ which ++ " block the newtaxa keyword is specified, but no new taxa are given."
                                  ys -> if length (head ys) == numTaxa (head xs)
                                        then Just $ "In the " ++ which ++ " block the number of new taxa does not match the number of taxa specified."
                                        else Nothing
                               else Nothing
  where
    which = if aligned pseq
            then "characters"
            else "unaligned"

getTaxaFromSeq :: PhyloSequence -> [String]
getTaxaFromSeq pseq 
    | areNewTaxa pseq = case seqTaxaLabels pseq of
                         []    -> {- trace (show keys) $ -} keys
                         (x:_) -> x
    | otherwise        = []
    where
        keys = M.keys $ getTaxaFromMatrix pseq

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
        _      <- blockend
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
-- and a StepMatrix, which is some metadata: the matrix name and the dimension,
-- as well as a TCMParseResult
tcmMatrixDefinition :: (Show s, MonadParsec s m Char) => m StepMatrix
tcmMatrixDefinition = do 
        _            <- symbol $ string' "usertype"
        matrixName   <- symbol $ somethingTill spaceChar
        _            <- symbol $ somethingTill $ char '='
        cardinality  <- symbol $ integer -- (stringDefinition "(stepmatrix)" <|> stringDefinition "(realmatrix)")
        mtxAlphabet  <- alphabetLine whitespaceNoNewlines
        assumpMatrix <- matrixBlock whitespaceNoNewlines
        pure $ StepMatrix matrixName (fromEnum cardinality) (TCM mtxAlphabet assumpMatrix)

treeBlockDefinition :: (Show s, MonadParsec s m Char) => m TreeBlock
treeBlockDefinition = {-do
    x <- getInput
    trace ("treeBlockDefinition"  ++ show x) $ -}do
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
             <|> (Matrix    <$> try matrixDefinition)
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

matrixDefinition :: (Show s, MonadParsec s m Char) => m [String]
matrixDefinition = {-do
    x <- getInput
    trace ("\n\nmatrixDefinition "  ++ show x) $ -}do
    _         <- symbol $ string' "matrix"
    goodStuff <- some   $ somethingTill c <* c
    _         <- symbol $ char ';'
    pure $ trace "Finished ignoredSubBlockDef\n" $ filter (/= "") goodStuff
    where 
        c = whitespaceNoNewlines *> (char ';' <|> endOfLine) <* whitespace

-- | ignoredSubBlockDef takes any string that terminates with
-- the passed end character, a semicolon or "end;". It returns that string up to, but
-- not including, whatever the terminating char is. Also fails if the input is "end;"
-- A test exists in the test suite.
ignoredSubBlockDef :: (Show s, MonadParsec s m Char) => Char -> m String
ignoredSubBlockDef endChar = {-do
    x <- getInput
    trace (("\n\nignoredSubBlockDef endChar: " ++ [endChar])  ++ show x) $ -}do
    _     <- notFollowedBy (space *> blockend) <?> "something other than end of block"
    stuff <- somethingTill stopMark
    _     <- stopMark
    pure stuff
  where
    stopMark = symbol $ char ';' <|> char' endChar

-- -------------------------------------------------------------------------------------------------
-- | Partitioning functions, which take a list of some type and produce a tuple.
-- Where there is a block with multiple optional fields or a field with multiple optional
-- subfields these take the output and put it into a tuple which can then be decomposed
-- and its fields used as arguments to a constructor.
-- I'm wondering if there's isn't a more efficient way to do this.
-- Also, can these be reduced to a single function, since they're all doing the same thing?

partitionAssumptionBlock :: [AssumptionField] -> [StepMatrix]
partitionAssumptionBlock = foldr f ([])
    where
        f (TCMMat n) vs = n:vs
        f (IgnAF  _) ws = ws

partitionSequenceBlock :: [SeqSubBlock] -> ([[String]],[CharacterFormat],[DimensionsFormat],[String],[[String]])
partitionSequenceBlock = foldr f ([],[],[],[],[])
    where
        f (Matrix    e)  (v,w,x,y,z) = (e:v,   w,   x,   y,   z)
        f (Format    e)  (v,w,x,y,z) = (  v, e:w,   x,   y,   z)
        f (Dims      e)  (v,w,x,y,z) = (  v,   w, e:x,   y,   z)
        f (Eliminate e)  (v,w,x,y,z) = (  v,   w,   x, e:y,   z)
        f (Taxa      e)  (v,w,x,y,z) = (  v,   w,   x,   y, e:z)
        f _                       ws = ws

partitionTaxaBlock :: [SeqSubBlock] -> (Int, [String])
partitionTaxaBlock = foldr f (0,[])
    where
        f (Dims n) (_,z) = (num, z)
            where
                num = numTaxa n
        f (Taxa n) (y,_) = (  y, n)
        f _           ws = ws

partitionNexusBlocks :: [NexusBlock] -> ([PhyloSequence], [TaxaSpecification], [TreeBlock], [AssumptionBlock], [IgnBlock])
partitionNexusBlocks = foldr f ([],[],[],[],[])
  where
    f (CharacterBlock   n) (xs,ys,zs,as,bs) = (n:xs,   ys,   zs,   as,   bs)
    f (TaxaBlock        n) (xs,ys,zs,as,bs) = (  xs, n:ys,   zs,   as,   bs)
    f (TreesBlock       n) (xs,ys,zs,as,bs) = (  xs,   ys, n:zs,   as,   bs)
    f (AssumptionsBlock n) (xs,ys,zs,as,bs) = (  xs,   ys,   zs, n:as,   bs)
    f (SkippedBlock     n) (xs,ys,zs,as,bs) = (  xs,   ys,   zs,   as, n:bs)
    --f _                                  ws = ws

partitionCharFormat :: [CharFormatField] -> (String, Either String [String], Either String [String], String, String, String, String, Bool, Bool, Bool, Bool, Bool)
partitionCharFormat = foldr f ("", Right [""], Right [""], "", "", "", "", False, False, False, False, False)
    where
        f (CharDT      n) (_,q,r,s,t,u,v,w,x,y,z,o) = (n,q,r,s,t,u,v,w,x,y,z,o)
        f (SymStr      n) (p,_,r,s,t,u,v,w,x,y,z,o) = (p,n,r,s,t,u,v,w,x,y,z,o)
        f (EqStr       n) (p,q,_,s,t,u,v,w,x,y,z,o) = (p,q,n,s,t,u,v,w,x,y,z,o)
        f (MissStr     n) (p,q,r,_,t,u,v,w,x,y,z,o) = (p,q,r,n,t,u,v,w,x,y,z,o)
        f (GapChar     n) (p,q,r,s,_,u,v,w,x,y,z,o) = (p,q,r,s,n,u,v,w,x,y,z,o)
        f (MatchChar   n) (p,q,r,s,t,_,v,w,x,y,z,o) = (p,q,r,s,t,n,v,w,x,y,z,o)
        f (Items       n) (p,q,r,s,t,u,_,w,x,y,z,o) = (p,q,r,s,t,u,n,w,x,y,z,o)
        f (RespectCase n) (p,q,r,s,t,u,v,_,x,y,z,o) = (p,q,r,s,t,u,v,n,x,y,z,o)
        f (Tokens      n) (p,q,r,s,t,u,v,w,_,y,z,o) = (p,q,r,s,t,u,v,w,n,y,z,o)
        f (Transpose   n) (p,q,r,s,t,u,v,w,x,_,z,o) = (p,q,r,s,t,u,v,w,x,n,z,o)
        f (Interleave  n) (p,q,r,s,t,u,v,w,x,y,_,o) = (p,q,r,s,t,u,v,w,x,y,n,o)
        f (Unlabeled   n) (p,q,r,s,t,u,v,w,x,y,z,_) = (p,q,r,s,t,u,v,w,x,y,z,n)
        f (IgnFF       _)                        ws = ws

partitionTreeBlock :: [TreeField] -> ([[String]], [(String,[NewickForest])])
partitionTreeBlock = foldr f ([],[])
    where
        f (Translation n) (ys,zs) = (n:ys,   zs)
        f (Tree n)        (ys,zs) = (  ys, n:zs)
        f _                    ws = ws

trimmed :: (Show s, MonadParsec s m Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace

-- This now imported from TCM
--symbol :: (Show s, MonadParsec s m Char) => m a -> m a
--symbol x = x <* whitespace

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

lstrip :: String -> String
lstrip "" = ""
lstrip input = dropWhile (`elem` " \t\n\r") input

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

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


