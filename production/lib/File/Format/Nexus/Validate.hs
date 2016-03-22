----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nexus.Validate
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for validating Nexus files.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Validate where

import           Data.Char              (isSpace,toLower)
import           Data.Either            (lefts) 
import           Data.Foldable          (toList)
import           Data.List              (sort, sortBy)
import           Data.List.Split        (splitOn)
import qualified Data.Map.Lazy     as M
import           Data.Maybe             (fromJust, catMaybes, maybeToList)
import           Data.Ord               (comparing)
import qualified Data.Set          as S (fromList, union)
import qualified Data.Vector       as V
import           File.Format.Nexus.Data
import           Safe
import           Text.Megaparsec.Prim   (MonadParsec)
import           Text.Megaparsec.Custom


-- Because some errors are dependent on passing some other validation, they've been split into
-- dependant and independant errors. Errors are enumerated below, along with their dependancies.
-- Comments are included in the code that explain which of the below errors are caught at various points.
-- Note that nos. 4--6, below, are only dependent on 3, so are simply done as nested ifs. This may
-- or may not be best practice.                                                             coded as
-- Errors currently caught: # error                                    err/warn   dependency   inde- or de-pendant    finished

--  ** removed ** too many sequence blocks                                            -              indep              done
--     There are actually two subcases here:       
--        too many aligned, too many unaligned       
--  ** removed ** No sequence blocks                                                  -              indep              done
       
--  1. No usable blocks                                                 error         -              indep              done
-----------------------------------  Everything below this relies on 1  -------------------------------------
--  2. Too many taxa blocks                                             warn          -              indep              done
--  3. No taxa specified---multiple checks, in multiple blocks          error         -              indep          not done
--  4. No seqMatrix in some sequence block                              warn          -              indep              done
--  5. No dimensions in some sequence block                             error         -              indep              done
--  6. Non-data sequence block with "nolabels", or data block           error         -              indep              done
--     but no taxa block _and_ no newtaxa specified in  
--     sequence blocks                                       
--  7. "newtaxa" keyword in characters or unaligned block,              error         -                dep              done 
--     but "nolabels" and no "taxlabels" subblock         
--  8. "newtaxa" keyword in characters or unaligned block,              warn          5                dep              done 
--     but ntaxa missing                                  
--  9. "nolabels" but labels actually present---
--     subsumed under 10, 11, 12                                        n/a           -                dep
-- 10. Matrix has alphabet values not specified                         error         4                dep
-- 11. Aligned block not Aligned                                        error         4                dep
-- 12. Matrix has non-spec'ed taxa                                      error         4,5              dep              revisit
-- 13. Matrix interleaved, but some blocks missing taxa                 error         4,5              dep              done
--     ---for aligned matrices, caught by 10
--     ---for unaligned, caught by combo of 12 & 22
-- 14. Matrix interleaved, unlabeled, but length not a                  error         4                dep              done 
--     multiple of # of taxa                          
-- 15. Character count is incorrect                                     error         4,5              dep
-- 16. Taxa count is incorrect                                          error         5                dep              done
--     a) in taxa block                                                 
--     b) in taxlabels in seq. block                                    
--     b) in matrix in sequence block                                   
-- 17. "nolabels" but there is a taxa block and newtaxa in seq block,   error         7,8              dep          not done
--     so order of sequences is unclear  
-- 18. Missing semicolons                                               error         -              indep         caught in parse
-- 19. Equate or symbols strings missing their closing quotes           error         3                dep         caught in parse (but ignored?)
-- 20. Match character can't be a space                                 error         3                dep          not done
-- 21. Space in taxon name                                              error         5,6              dep              done
--     ---for aligned, should be caught by 16
--     ---for unaligned, caught by combo of 12 & 22
-- 22. In unaligned, interleaved block, a taxon is repeated             error         3                dep              done
-- 23. Since whitespace is allowed in sequences, and \n counts
--     as whitespace, so I need to eliminate that. At the same time,
--     it seems to be possible to list taxa *only* inside the 
--     character matrix. In that specific case, I can't

-- TODOs:
-- • Check for info on spaces in seqs. Remove them from non-continuous data.
-- • Verify character metadata, especially alphabets
-- • deal with special chars in step matrices
-- • check for and eliminate thusly noted characters (in at least two places?)
-- • add error checking on equate string (it needs to be of form a=b, [c=d]) ** also, look up Bachus-Naur form again.
-- • ignore case
-- • check alignment and length of aligned blocks
-- • check tcm for size; see how *'s work in tcm
-- • check for chars that aren't in alphabet
-- • fail on incorrrect datatype
-- • dependentErrors & independentErrors become :: String error, String warning => [Maybe (Either error warning)]
     -- then partitionEithers . catMaybes, etc., etc. (talk to Alex)
-- • warn that eliminate doesn't work on unaligned data
-- • capture data on ordered chars
-- • check for gap treatment, possibly replace with missing

validateNexusParseResult :: (Show s, MonadParsec s m Char) => NexusParseResult -> m Nexus
validateNexusParseResult (NexusParseResult inputSeqBlocks taxas treeSet assumptions _ignored) 
  | null inputSeqBlocks && null taxas && null treeSet = fails ["There are no usable blocks in this file."] -- error 1
  | not (null independentErrors)                      = fails independentErrors
  | not (null dependentErrors)                        = fails dependentErrors
  | otherwise                                         = case maybeThing of
                                                              Left   err               -> fail err
                                                              Right (outputSeqTups, _) -> pure $ Nexus {-taxaLst-} outputSeqTups 
  where
      -- Ordered by call, so first independentErrors, then dependentErrors, then outputSeqTups. Dependencies are subgrouped according to calling fn.
      
      -- Independent errors
        independentErrors  = catMaybes $ noTaxaError : multipleTaxaBlocks : seqMatrixDimsErrors ++ wrongDataTypeErrors-- sequenceBlockErrors
      -- types of independent errors
        taxaDimsMissingError = taxaDimsMissing taxas inputSeqBlocks
        noTaxaError        = if null taxas && not (foldr (\x acc -> acc || areNewTaxa x) False inputSeqBlocks) -- will register as False if inputSeqBlocks is empty
                             then Just "Taxa are never specified. \n"  {-++ (show taxas) ++ " " ++ (show inputSeqBlocks) -} -- error 6
                             else Nothing
        multipleTaxaBlocks = case taxas of
                             (_:_:_) -> Just "Multiple taxa blocks supplied. This is unclear input, and not allowed.\n"  -- error 1
                             _       -> Nothing
        seqMatrixDimsErrors = foldr (\x acc -> seqMatrixMissing x : acc) [] inputSeqBlocks ++  -- Note that seqMatrixDimsErrors will be [] if 
                                                                                               -- there are no character, unaligned or data blocks.
                                                                                               -- This allows us to do tree manipulations in their absence.
        
                              foldr (\x acc -> seqDimsMissing x : acc) [] inputSeqBlocks 
        wrongDataTypeErrors = foldr (\x acc -> wrongDataType x : acc) [] inputSeqBlocks

      -- Dependent errors
        dependentErrors = catMaybes $ incorrectTaxaBlockCount : ({- seqDimsError ++ -} missingCloseQuotes ++ seqTaxaCountErrors {- ++ {- mtxTaxonCountErrors ++ TODO: decide if I can really delete this and seqDimsError and incorrectCharCount. -} incorrectCharCount -} )
      
      -- types of dependent errors
        incorrectTaxaBlockCount = f taxas >>= \(TaxaSpecification num listedTaxa) -> 
            if num /= length listedTaxa
                then Just "Incorrect number of taxa in taxa block.\n" {- ++ (show num) ++ " " ++ (show taxons) -} -- error 16a
                else Nothing
            where
                f [x] = Just x
                f _   = Nothing
        --seqDimsError        = map mtxTax inputSeqBlocks 
          --where
          --   f (PhyloSequence _ mat _ dim _ _ _) = mat 
        missingCloseQuotes  = map Just (lefts equates) ++ map Just (lefts symbols') -- error 19
        seqTaxaCountErrors  = foldr (\x acc -> checkForNewTaxa x : acc) [] inputSeqBlocks -- errors 7, 8, 16b
        mtxTaxonCountErrors = foldr (\x acc -> getMatrixTaxonRecurrenceErrors x ++ acc) [] inputSeqBlocks -- errors 12, 22
 --       incorrectCharCount  = checkSeqLength (filter (\x -> alignedSeq x) inputSeqBlocks) outputSeqTups -- TODO: This doesn't work, because it takes an entire list of PhyloSequence blocks and the complete, concatted sequences. It needs to take place at a different point in the process.

      -- dependencies for dependent errors
        equates = foldr (\x acc -> getEquates x : acc) [] inputSeqBlocks
        symbols' = foldr (\x acc -> getSymbols x : acc) [] inputSeqBlocks
        taxaLst  = if not $ null taxas
                      then V.fromList $ taxaLabels $ head taxas
                      else V.empty

      -- these are still dependencies for dependent errors, but they're also the beginning of the output gathering.                  
        maybeThing = foldSeqs <$> seqMetadataTuples
        --seqMetadataTuples = map (\singleSeq -> ( getSeqFromMatrix singleSeq taxaLst
        --                          , getCharMetadata costMatrix singleSeq
        --                          )) inputSeqBlocks -- TODO: replace getSeqFromMatrix blah blah with parsedSeqs
        costMatrix = headMay . tcm =<< headMay assumptions -- TODO: why does this work?
        
        seqMetadataTuples = createSeqMetaTuples <$> parsedSeqs
        createSeqMetaTuples = map (\(taxonSeqMap,rawSequence) -> (taxonSeqMap, getCharMetadata costMatrix rawSequence)) . (`zip` inputSeqBlocks)
        parsedSeqs = decisionTree inputSeqBlocks taxaLst
        -- taxaSeqVector = V.fromList [(taxon, alignedTaxaSeqMap M.! taxon) | taxon <- taxaLst]
        --unalignedTaxaSeqMap = getSeqFromMatrix (getBlock "unaligned" inputSeqBlocks) taxaLst

---------------------------  Following set of fns is actually set of nested ifs to match decision tree in docs  ---------------------------
-------------------------  Mostly, these fns just check for errors much of the logic is dup'd in getSeqFromMatrix  ------------------------

decisionTree :: [PhyloSequence] -> V.Vector String -> Either String [TaxonSequenceMap]
decisionTree inputSeqBlocks taxaLst = 
    if null inputSeqBlocks
        then Right [M.empty]
        else mapM (\x -> isItTransposed x taxaLst) inputSeqBlocks

isItTransposed :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
isItTransposed block taxaLst  = 
        if any transpose $ format block
            then Left "Uh-oh there's a transposed block. That was very sneaky of you but I saw you!!!" -- TODO: Nice errors, handle transposed case
            else handleNontransposedSeqs block taxaLst

handleNontransposedSeqs :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleNontransposedSeqs block taxaLst = 
    if any unlabeled $ format block
        then handleUnLabeledSeqs block taxaLst
        else handleLabeledSeqs   block taxaLst

handleUnLabeledSeqs :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleUnLabeledSeqs block taxaLst = if any interleave $ format block
        then Left "Interleaved blocks must be labeled.\n"
        else handleUnlabeledNotInterleavedSeqs block taxaLst

handleLabeledSeqs :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleLabeledSeqs block taxaLst =
    if any interleave $ format block
        then Right $ getSeqFromMatrix block taxaLst
        else handleLabeledNotInterleavedSeqs block taxaLst

handleLabeledNotInterleavedSeqs :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleLabeledNotInterleavedSeqs block taxaLst =
    if alignedSeq block
        then Right $ getSeqFromMatrix block taxaLst
        else Left $ "The " ++ (blockType block) ++ " block is labeled, not aligned, and not interleaved. Since the Nexus spec dictates that whitespace (hence line returns) in non-interleaved sequence matrices is ignored, it is impossible to discriminate between sequences and taxon names."

handleUnlabeledNotInterleavedSeqs :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleUnlabeledNotInterleavedSeqs block taxaLst = 
    if null taxaLst && null taxlabels'
        then Left $ "In a " ++ (blockType block) ++ " block there is an unlabeled matrix which seems not to have any corresponding taxa.\n"
        else handleUnlabeledNotInterleavedSeqsWithSepTaxa block taxaLst taxlabels'
    where
        taxlabels' 
            | null $ seqTaxaLabels block = V.empty
            | otherwise = V.fromList $ head $ seqTaxaLabels block

handleUnlabeledNotInterleavedSeqsWithSepTaxa :: PhyloSequence -> V.Vector String -> V.Vector String -> Either String TaxonSequenceMap
handleUnlabeledNotInterleavedSeqsWithSepTaxa block taxaLst taxlabels' =
    if not (null taxaLst) && not (null taxlabels')
       then Left $ "In a " ++ (blockType block) ++ " block there is an unlabeled matrix, but there are both newtaxa defined in the block, and there is a separate taxa block. Thus, the ordering of the sequences in this block is unclear.\n"
       else if null taxaLst
               then handleUnlabeledCheckTaxaCardinality block taxaLst
               else handleUnlabeledCheckTaxaCardinality block taxlabels'

handleUnlabeledCheckTaxaCardinality :: PhyloSequence -> V.Vector String -> Either String TaxonSequenceMap
handleUnlabeledCheckTaxaCardinality block taxaLst =
    if length taxaLst /= mtxLength
       then Left $ "In a " ++ (blockType block) ++ " block, either the number of taxa or the number of sequences in incorrect. " ++ show (length taxaLst) ++ " are given, but there are " ++ show mtxLength ++ " sequences in the matrix.\n"
       else Right $ getSeqFromMatrix block taxaLst
    where mtxLength = length $ head $ seqMatrix block -- this is safe, as we've already checked to make sure this blcok has a matrix.

------------------------------------------------------  End decision tree logic  ------------------------------------------------------


-- | foldSeqs takes a list of tuples of sequence maps and character metadata, and
-- returns a tuple containing a single Sequences tuple and an Int. (The Int is only used for recursive calls.)
foldSeqs :: [(TaxonSequenceMap,V.Vector CharacterMetadata)] -> (Sequences, Int)
foldSeqs []     = ((M.empty, V.empty), 0)
foldSeqs ((taxSeqMap,charMDataVec):xs)   = ((newSeqMap, newMetadata), totLength)
    where 
        ((curMap,curMetadata),curLength) = foldSeqs xs
        newSeqMap                        = M.unionWith (updateSeqInMap curLength) taxSeqMap curMap
                                           -- This condition shouldn't ever be True. Only seqs should have missing data.
        newMetadata                      = charMDataVec V.++ if length curMetadata < curLength 
                                                                then V.fromList [] V.++ curMetadata -- TODO: Error out here?
                                                                else curMetadata
        totLength                        = curLength +  V.length charMDataVec
    

-- | updateSeqInMap takes in a TaxonSequenceMap, a length (the length of the longest sequence in the map), a taxon name and a sequence.
-- It updates the first map by adding the new seq using the taxon name as a key. If the seq us shorter than the max, it is first
-- buffered by a vector of Nothing.
-- This buffering is okay because unaligned sequences, under dynamic homology, are treated as a single sequences. Therefore the buffering 
-- does not cause and "alignment". TODO: Make sure the reasoning is right in this, regarding buffering
updateSeqInMap :: Int -> Sequence -> Sequence -> Sequence
updateSeqInMap curLength inputSeq curSeq = newSeq
    where
        newSeq        = seqToAdd V.++ curSeq
        missingLength = curLength - length curSeq
        emptySeq      = V.replicate missingLength Nothing
        seqToAdd      = inputSeq V.++ emptySeq

-- | wrongDataType looks at a PhyloSequence block and determines if it has a valid data type. If the datatype is missing, it will return Nothing, as
-- results in default behavior ("standard" is default).
-- TODO: ask Ward if mixed datatype is allowed (hope to god no).
wrongDataType :: PhyloSequence -> Maybe String
wrongDataType inSeq =
    if (map toLower dataType) `elem` ["standard", "dna", "rna", "nucleotide", "protein", "continuous"]
        then Nothing
        else Just $ dataType ++ " is not an accepted type of data.\n"
    where
        (_, _noLabels, _interleaved, _tkns, dataType, _matchChar') = getFormatInfo inSeq


-- | checkSeqLength takes in the list of PhyloSequences and the final map of sequences and checks each sequence to see whether it's
-- aligned, and if so whether it 
checkSeqLength :: [PhyloSequence] -> Sequences -> [Maybe String]
checkSeqLength [] _            = [Nothing]
checkSeqLength seqBlockLst (seqMap,_) = 
    M.foldrWithKey (\key val acc -> (if length val == len
                                     then Nothing
                                     else Just (key ++ "'s sequence is the wrong length in an aligned block. It should be " ++ show len ++ ", but is " ++ show (length val) {- ++ ":\n" ++ show val -} ++ "\n")) : acc) [] seqMap
    where
        len = numChars . head . charDims $ head seqBlockLst -- TODO: fix this line

-- | taxaDimsMissing tries to determine whether any ntax are missing. They could be missing from either
-- a Taxa block, or from a Data block, or from a Characters or Unaligned block that has newtaxa.
-- TODO: fix this tornado code.
-- TODO: write down this decision tree.
taxaDimsMissing :: [TaxaSpecification] -> [PhyloSequence] -> [Maybe String]
taxaDimsMissing taxas inputSeqBlocks = taxaProblems ++ seqProblems
    where
        taxaProblems = map f taxas
        seqProblems = map g inputSeqBlocks
        f (TaxaSpecification len list) = let len' =length list 
            in if len == len' 
                then Nothing
                else Just $ "ntax incorrect in Taxa block. Found: " ++ show len' ++ ", but ntax is given as " ++ show len ++ ".\n"
        g (PhyloSequence _ _ _ dimensions _ _ _ blockType' ) = 
            if blockType' == "data" -- TODO: check for case-insensitivity problems
                then
                    if not (null dimensions)
                        then 
                            let (DimensionsFormat _ ntax' _) = head dimensions
                            in 
                                if ntax' == 0 -- 0 is default value if ntax is missing
                                    then Just "Data block is missing ntax directive.\n" 
                                    else Nothing
                        else Just "Data block is missing dimensions.\n"
                else 
                    if not (null dimensions)
                        then 
                            let (DimensionsFormat newtaxa' ntax' _) = head dimensions
                            in 
                                if newtaxa' && ntax' == 0 -- 0 is default value if ntax is missing
                                    then Just $ blockType' ++ " block is missing ntax directive.\n" 
                                    else Nothing
                        else Just $ blockType' ++ "Data block is missing dimensions.\n"




-- TODOs: 
-- • name chars, Nope don't do that here, just Maybe them!
-- • make sure I've dealt with dynamic & static homology correctly here.
-- • fix elims
-- • deal with additivity
-- • deal with weight
-- • deal with gapmode 
-- Note that these last four will all need the same functionality under the hood.
getCharMetadata :: Maybe StepMatrix -> PhyloSequence -> V.Vector CharacterMetadata
getCharMetadata mayMtx seqBlock = 
    V.replicate len $ CharacterMetadata "" aligned cType alph False mayTCM additivity wt
    where 
        aligned     = alignedSeq seqBlock
        cType       = read (charDataType form) :: CharDataType
        alph        = if areTokens form
                      then syms
                      else g $ headMay syms
        syms        = f $ symbols form
        f (Right x) = x
        f _         = [""] -- Shouldn't be possible, but leaving it in for completeness.
        g (Just s)  = foldr (\x acc -> [x] : acc) [] s
        g Nothing   = [""]
        form        = head $ format seqBlock
        len         = numChars . head $ charDims seqBlock
        mayTCM      = matrixData <$> mayMtx
        additivity  = False
        wt          = 1



-- | getMatrixTaxonRecurrenceErrors takes a Phylosequence. It reads throught the PhyloSequence matrix to see if there are any taxa
-- that appear the incorrect number of times (all should appear the same number of times)
getMatrixTaxonRecurrenceErrors :: PhyloSequence -> [Maybe String]
getMatrixTaxonRecurrenceErrors seq' = wrongCountErrors -- ++ extraTaxonErrors
    where
        wrongCountErrors = M.foldrWithKey (\key val acc -> (if val /= median
                                                               then Just ("\"" ++ key ++ "\" appears the wrong number of times in a sequeblock matrix. It should appear " ++ show median ++ " times, but it actually appears " ++ show val ++ " times.\n")
                                                               else Nothing) : acc
                                          ) [] seqTaxaMap
        seqTaxaMap       = getTaxaFromMatrix seq'
        median           = findMedian $ M.elems seqTaxaMap

-- | findMedian sorts a Vector of orderables, then returns the middle value. It assumes there is at least one element in the Vector.
-- TODO: change this to use Vectors; will require jumping through Monad hoops. See: http://stackoverflow.com/questions/3655329/how-does-one-sort-with-data-vector-generic-mutable
findMedian :: Ord a => [a] -> a
findMedian xs = sort xs !! quot (length xs) 2

-- | getEquates takes a PhyloSequence and returns an Either String [String]. The left is an error message received from the parser.
-- The right is a list of strings retrieved from the Parser. If the right is empty, it returns Right [""].
getEquates :: PhyloSequence -> Either String [String]
getEquates = maybe (Right [""]) equate . headMay . format

-- | getTaxaFromMatrix takes a PhyloSequence and returns a map from String to Int, where the String is a taxon name, and the Int is the number of times it appears in the sequence matrix.
getTaxaFromMatrix :: PhyloSequence -> M.Map String Int
getTaxaFromMatrix seq' = {-trace (show taxa) $ -}
    if noLabels
        then M.empty
        else taxaMap
    where
        (_, noLabels, _interleaved, _tkns, _type, _matchChar') = getFormatInfo seq'
        mtx     = head $ seqMatrix seq' -- I've already checked to make sure there's a matrix
        taxaMap = foldr (\x acc -> M.insert x (succ (M.findWithDefault 0 x acc)) acc) M.empty taxa
        taxa    = foldr (\x acc -> takeWhile (`notElem` " \t") x : acc) [] mtx

-- TODO: This is too similar to getEquates, above. Can they be combined?
getSymbols :: PhyloSequence -> Either String [String]
getSymbols = maybe (Right [""]) symbols . headMay . format

-- | splitSequenceReplaceAmbiguities takes in a String and returns a Sequence (Vector of Characters)
-- The String may be returned as a singleton Vector---a single character, if unaligned (dynamic homology), or 
-- as a Vector with length > 1 if aligned (static homology).
-- either way, multiple-character ambiguities are replaced by single character ambiguities.
-- TODO: abmiguity replacement shouldn't actually take place if continuous or if custom alphabet
splitSequenceReplaceAmbiguities :: Bool -> Bool -> Bool -> String -> Sequence
splitSequenceReplaceAmbiguities isTokens isContinuous isAlign seq' = finalList
    where 
        finalList = 
            if isAlign 
                then V.fromList $ (Just . V.singleton) <$> chars -- aligned, so each item in vector of ambiguity groups is single char
                else V.singleton . Just $ V.fromList chars -- not aligned, so whole vector of ambiguity groups is single char
        chars = 
            if isTokens || isContinuous
                then findAmbiguousTokens (words seq') [] False
                else findAmbiguousNoTokens (strip seq') [] False

-- | findAmbiguousNoTokens takes a sequence as a String. If it encounters a '{' or '(', it translates
-- the characters inside the delimiters into a list of Strings. It then outputs the original input
-- with all ambiguous sequences replaced by these lists
-- Parens and curly braces are treated the same
findAmbiguousNoTokens :: String -> AmbiguityGroup -> Bool -> [AmbiguityGroup]
findAmbiguousNoTokens [] _ _ = []
findAmbiguousNoTokens (x:xs) acc isAmb =
              case x of
                ' ' -> findAmbiguousNoTokens xs acc isAmb
                '{' -> findAmbiguousNoTokens xs [] True
                '}' -> acc : findAmbiguousNoTokens xs [] False
                '(' -> findAmbiguousNoTokens xs [] True
                ')' -> acc : findAmbiguousNoTokens xs [] False
                _   -> if isAmb
                           then findAmbiguousNoTokens xs (acc ++ [[x]]) isAmb
                           else [[x]] : findAmbiguousNoTokens xs [] isAmb

-- | seqMatrixMissing takes in a PhyloSequence and makes sure it has a sequence matrix. 
-- Returns a Bool. 
seqMatrixMissing :: PhyloSequence -> Maybe String
seqMatrixMissing phyloSeq
  | numMatrices < 1 = tooFew
  | numMatrices > 1 = tooMany
  | otherwise       = Nothing
  where
    numMatrices = length $ seqMatrix phyloSeq
    tooFew      = Just $ blockType phyloSeq ++ " block has no sequence matrix.\n"
    tooMany     = Just $ blockType phyloSeq ++ " block has more than one sequence matrix.\n"

-- | seqDimsMissing checks a PhyloSequence to make sure it has the requisite sequence dims. 
seqDimsMissing :: PhyloSequence -> Maybe String
seqDimsMissing phyloSeq
  | numDims < 1 = tooFew
  | numDims > 1 = tooMany
  | otherwise   = Nothing
  where
    numDims = length $ charDims phyloSeq
    tooFew  = Just $ blockType phyloSeq ++ " block has no dimensions.\n"
    tooMany = Just $ blockType phyloSeq ++ " block has more than one dimension directive.\n"

-- | findAmbiguousTokens is similar to findAmbiguousNoTokens. It takes a sequence as a String. 
-- If it encounters a '{' or '(', it translates 
-- the characters inside the delimiters into a list of Strings. It then outputs the original input
-- with all ambiguous sequences replaced by these lists
findAmbiguousTokens :: [String] -> AmbiguityGroup -> Bool -> [AmbiguityGroup]
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
safeHead []    = []
safeHead (x:_) = [x]

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

-- | findInterleaveError takes a Vector of the taxon names and a Phylosequence and then attempts to determine
-- whether the sequence matrix in the PhyloSequence, if interleaved, is interleaved correctly. I.e., no taxon appears
-- more or fewer times than another.
findInterleaveError :: V.Vector String -> PhyloSequence -> Maybe String
findInterleaveError taxaLst seq' 
    | taxaLst == V.empty = Nothing
    | otherwise          =
        -- Line returns are not ignored in interleaved blocks, so this is correct logic.
        if interleaved && noLabels && (lineCount `mod` taxaCount /= 0)
            then Just $ which ++ " block is not interleaved correctly. \n" {- ++ (show numLines) ++ " " ++ (show numTaxa) -}
            else Nothing
        where
            formatted   = not . null $ format seq'
            interleaved = formatted && interleave (head $ format seq')
            noLabels    = formatted && unlabeled  (head $ format seq')
            taxaCount   = length taxaLst
            lineCount   = length . head $ seqMatrix seq'
            which       = blockType seq'

getFormatInfo :: PhyloSequence -> (Bool, Bool, Bool, Bool, String, String)
getFormatInfo phyloSeq = case headMay $ format phyloSeq of
                       Nothing -> (False, False, False, False, "", "")
                       Just x  -> ( alignedSeq phyloSeq 
                                  , unlabeled x
                                  , interleave x
                                  , areTokens x
                                  , map toLower (charDataType x)
                                  , matchChar x
                                  )

-- | getSeqFromMatrix takes a PhyloSequence and a Vector of taxon names and produces a Map from 
-- taxon name to Sequence.
-- In doing so, it replaces match characters, equates characters and 
-- This function was written prior to creating the decision tree logic used to create the "handle" fns., and
-- prior to realizing that newline characters are ignored whitespace except when the matrix is interleaved.
-- That decision tree was created for use under deadline pressure, so to save time the existing fn 
-- was simply modified to be able to handle all three use cases present in the tree, leading to a rather
-- convoluted structure. Those use cases are:
-- 1.   labeled,     interleaved,     aligned
-- 2.   labeled,     interleaved, not aligned
-- 3.   labeled, not interleaved,     aligned
-- 4. unlabeled, not interleaved,     aligned
-- 5. unlabeled, not interleaved, not aligned
-- There are actually subcases relating to whether the sequences are tokenized, as well, but they're not as 
-- important to my short-term goal, so they're in the code, but not separated out in the comments.
--
-- All checking to make sure that error cases don't exist must be performed prior to calling this fn, so some
-- seemingly important program logic does not exist here, but occurs, rather in the "handle" fns.
getSeqFromMatrix :: PhyloSequence -> V.Vector String -> TaxonSequenceMap
-- getSeqFromMatrix [] _ = V.empty
getSeqFromMatrix seqBlock taxaLst =
    M.map (splitSequenceReplaceAmbiguities tkns isCont aligned) equatesReplaced
    where
        (aligned, noLabels, interleaved, tkns, charType, matchChar') = getFormatInfo seqBlock
        seqLen = numChars $ head $ charDims seqBlock -- I've already checked to make sure there's a dimensions in the block
        taxaCount  = length taxaLst
        taxaMap    = M.fromList . zip (V.toList taxaLst) $ repeat ""
        mtx        = head $ seqMatrix seqBlock -- I've already checked to make sure there's a matrix
        entireSeqs = if noLabels    -- entireSeqs will be a list of tuples (taxon, concatted seq)
                         then zip (V.toList taxaLst) mtx -- Cases 4 and 5: Alignment will be checked in splitSequenceReplaceAmbiguities
                         else if interleaved 
                              then -- Cases 1 and 2: Alignment will be checked in splitSequenceReplaceAmbiguities
                                  getTaxonAndSeqFromMatrixRow <$> mtx 
                              else -- Case 3: To get here, we must have already errored out unaligned in "handle" fns
                                  getTaxaAndSeqsFromEntireMatrix (isCont || tkns) seqLen mtx -- TODO: need to add custom alphabet to condition here?
        entireDeinterleavedSeqs = if interleaved
                                  then deInterleave taxaMap entireSeqs
                                  else M.fromList entireSeqs
        firstSeq = fromJust $ M.lookup (if noLabels
                                        then taxaLst V.! 0
                                        else takeWhile (`notElem` " \t") $ head mtx)
                            entireDeinterleavedSeqs
        matchCharsReplaced = if matchChar' /= "" && aligned -- TODO: Do I need to do this on tokens, continuous and custom alphabets?
                             then M.map (replaceMatches (head matchChar') firstSeq) entireDeinterleavedSeqs
                             else entireDeinterleavedSeqs
        --vectorized = V.fromList 
        equatesReplaced = if head eqStr /= "" && not tkns && not isCont -- TODO: Also don't do on custom alphabets?
                            then M.map (replaceEquates eqMap) matchCharsReplaced
                            else matchCharsReplaced
        eqStr = either (\x -> [""]) (\x -> id x) $ getEquates seqBlock
        eqMap = M.fromList $ map (\xs -> (head xs, tail $ dropWhile (\x -> not $ '=' == x) xs) ) eqStr -- TODO: force equates string to be properly formatted
        isCont = charType == "continuous"

-- | deInterleave takes in a Map String String, where the first String is a taxon label, as well as an 
-- interleaved seqMatrix in the form [(taxon,sequence)] and returns
-- a seqMatrix of the form Map taxon sequence. The original list of tuples should have duplicate taxon entries
-- (because of the seqMatrix being interleaved), and the seqs should be concatted---in order---in the 
-- returned map.
-- The first 'Map' parameter is used for when there are no label present
-- Note that this fn is O(n) where n is the total length of the sequence, and not O(n^2), as originally feared.
-- A (partial?) test exists in the test suite.
deInterleave :: M.Map String String -> [(String, String)] -> M.Map String String
deInterleave = foldr (\(seqName, phyloSeq) acc -> M.insertWith (++) seqName phyloSeq acc) 

-- | getTaxonAndSeqFromMatrixRow takes a String of format "xxx[space or tab]yyy"
-- and returns a tuple of form ("xxx","yyy")
-- A test exists in the test suite.
getTaxonAndSeqFromMatrixRow :: String -> (String, String)
getTaxonAndSeqFromMatrixRow inStr = (seqName, phyloSeq)
    where 
        (seqName, rest) = span   (`notElem` " \t") inStr
        phyloSeq        = dropWhile (`elem` " \t") rest

-- | getTaxaAndSeqsFromEntireMatrix takes an int, the length of the sequence, and a sequence matrix. It then builds
-- a list of (String, String), where the first in each tuple is a taxon name, and the second is the corresponding sequence. 
-- To do this, it needs to count each phylogenetic character, because it doesn't actually know where the sequence
-- ends (as newlines are whitespace, and therefore should be ignored).

-- first, prime loop with first line
-- acc is [(taxon,seq)]
-- curLen is length seq
-- while there's something left:
    -- line = words input
    -- if curLen < seqLen:
        -- if tkns or cont or custom alphabet:
            -- newLine = line
        -- else:
            -- newLine = concat line
        -- prepend newLine to snd $ head accumulator
        -- curLen += length newLine
    -- else:
        -- taxon = head line
        -- if tkns or cont or custom alphabet:
            -- newLine = tail line
        -- else:
            -- newLine = concat $ tail line
        -- curSeq = newLine
        -- add (taxon, curSeq) to list
        -- curLen = length newLine
        -- curSeq = newLine
getTaxaAndSeqsFromEntireMatrix :: Bool -> Int -> [String] -> [(String, String)] 
getTaxaAndSeqsFromEntireMatrix tokenized seqLen mtx =
    getTaxaAndSeqsFromEntireMatrixHelper tokenized seqLen curLen (tail mtx) accLst
    where
        accLst = [(taxon,seq)]
        curLen = if tokenized
                    then length $ tail line
                    else length seq -- this removes extra spaces in non-tokenized lines
        taxon = head line
        seq = if tokenized
                 then unwords $ tail line
                 else concat $ tail line
        line = words $ head mtx -- Should already have failed if mtx is empty

getTaxaAndSeqsFromEntireMatrixHelper :: Bool -> Int -> Int -> [String] -> [(String, String)] -> [(String, String)]
getTaxaAndSeqsFromEntireMatrixHelper tokenized seqLen curLen mtx acc 
    | null mtx = acc
    | curLen < seqLen =
        let 
            newAcc = newTup : tail acc
            newLen = curLen + if tokenized
                                 then length line
                                 else length seq -- this removes extra spaces in non-tokenized lines
            seq = if tokenized 
                     then ' ' : unwords line
                     else concat line
            line = words $ head mtx -- Safe because of first pattern match.
            newTup = (fst $ head acc, headSeq)
            headSeq = (snd $ head acc) ++ seq
        in getTaxaAndSeqsFromEntireMatrixHelper tokenized seqLen newLen (tail mtx) newAcc
    | otherwise = 
        let 
            newTup = (taxon,seq)
            newLen = if tokenized
                        then length $ tail line
                        else length seq -- this removes extra spaces in non-tokenized lines
            taxon = head line
            seq = if tokenized 
                     then ' ' : (unwords $ tail line)
                     else concat $ tail line
            line = words $ head mtx -- Safe because of first pattern match
        in getTaxaAndSeqsFromEntireMatrixHelper tokenized seqLen newLen (tail mtx) (newTup : acc)

-- | replaceEquates takes a Map Char String, and a String. For each Char in the input String
-- it looks up that value in the Map. If the key exists, that position in the String gets replaced by 
-- the map value. Otherwise, the original value remains.
-- Abstracted Char to a as an exercise.
replaceEquates :: (Ord a) => M.Map a [a] -> [a] -> [a]
replaceEquates eqMap inputSeq = foldr (\x acc -> (M.findWithDefault [x] x eqMap) ++ acc) [] inputSeq

-- | replaceMatches takes a match character and two Strings, 
-- a canonical String and a String that contains the match character. 
-- It returns the second string with all match characters replaced by 
-- whatever character is in the same position in the canonical string
-- O(n)
-- A test exists in the test suite.
replaceMatches :: Char -> String -> String -> String
replaceMatches matchTarget canonical toReplace = {- trace (canonical ++"\n" ++ toReplace ++ "\n") $ -}
    zipWith f canonical toReplace
    where
        f x y = if y == matchTarget
                then x
                else y

-- | chunksOf is based on Text.chunksOf, but is more general.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = f : chunksOf n s
  where
    (f,s) = splitAt n xs

-- | areNewTaxa takes in a PhyloSequence (character, unaligned or data block) and returns 
-- True if that PhyloSequence contains new taxa, False otherwise.
-- The criteria for True are: 1) if it is a data block
--                            2) if a taxlabels sub-block is present
--                            3) if newtaxa is specified in the dimensions sub-block
-- By my reading of the Nexus spec, taxlabels only occur if newtaxa has, and if newtaxa is present, then taxlabels must be, 
-- but that's not how this fn currently works.
-- In our sample files there were several files that had data blocks with no newtaxa token, no taxa block, and taxon labels only
-- occurring as the first token in each line of the character matrix.
areNewTaxa :: PhyloSequence -> Bool
areNewTaxa phyloSeq
    | blockType phyloSeq == "data" = True
    | otherwise =
          case charDims phyloSeq of
            []  -> False
            xs  -> case seqTaxaLabels phyloSeq of
                    [] -> False
                    _  -> newTaxa $ head xs

--checkDims :: Phylosequence -> Maybe String
--checkDims seq = let dim = numchars $ head (charDims seq)
--                in if dim == length

-- | checkForNewTaxa is frighteningly similar to areNewTaxa, but I couldn't figure out a way around
-- having them both, because of the way that areNewTaxa is used. Unlike areNewTaxa, returns either Nothing or 
-- a list of taxon errors. These errors fall under two cases: newtaxa is spec'd, but no new taxa actually appear;
-- there are new taxa, but the number of new taxa doesn't match the the spec'd number.
-- TODO: this isn't right: ntax is the number of taxa in the matrix, not the number of *new* taxa in the matrix.
-- Also, conditions don't seem right: they should match areNewTaxa more closely.
-- See haddock notes for areNewTaxa for conditions.
checkForNewTaxa :: PhyloSequence -> Maybe String
checkForNewTaxa phyloSeq = case charDims phyloSeq of
                         [] -> Nothing
                         xs -> if newTaxa $ head xs
                               then
                                 case seqTaxaLabels phyloSeq of
                                  [] -> Just $ "In the " ++ which ++ " block the newtaxa keyword is specified, but no new taxa are given."
                                  ys -> if length (head ys) == numTaxa (head xs)
                                        then Just $ "In the " ++ which ++ " block the number of new taxa does not match the number of taxa specified."
                                        else Nothing
                               else Nothing
  where
    which = blockType phyloSeq

-- | createElimTups takes the output of sortElimsStr (so a list of strings) and returns a list of tuples of form (Bool, Int)
-- This will be used to determine whether a given character in an aligned sequence is to be ignored. However, because 
-- the elim clause in a Nexus file is of the form x,y-z,a,... it's possible to be within a range. So if a single number is given
-- The tuple will be (True,that number :: Int), otherwise, it will be (False, ending number of range :: Int)
createElimTups :: [String] -> [(Bool,Int)]
createElimTups = foldr (\x acc -> f x ++ acc) []
    where 
        f inStr = if secondNumStr /= ""
                      then [(True,firstNum),(False,secondNum)]
                      else [(True,firstNum)]
            where
                (firstNumStr,secondNumStr) = break (== '-') inStr
                firstNum = {- trace ("first: " ++ firstNumStr) $ -}read firstNumStr :: Int
                secondNum = {- trace ("second: " ++ secondNumStr) $ -}read (tail secondNumStr) :: Int

-- | getNext takes as input an index of a character in an aligned sequence, as well as the current index in a list of tuples 
-- as described in the output of createElimTups. It then determines whether the input character index should be ignored or not
-- and returns that determination as a Bool (ignored---or eliminated, in Nexus terminology---is True), 
-- along with an Int giving the updated index to check at when getNext is next run.
-- This could probably be done more nicely with a state monad, but I don't know, yet, how to use those.
-- Note that sequences are indexed starting at 1, but Haskell Vectors are indexed starting at 0.
getNext :: Int -> Int -> V.Vector (Bool,Int) -> (Bool, Int)
getNext seqIdx elimsIdx toElim
    | elimsIdx >= V.length toElim         = (False, elimsIdx) -- there are no more things to eliminate
    | seqIdx == snd (toElim V.! elimsIdx) = (True, succ elimsIdx) -- The given input is specifically listed, and should be eliminated
    | seqIdx >  snd (toElim V.! elimsIdx) = (False, succ elimsIdx) -- The sequence index is > the current index, so we need to move forward in toElim
    | otherwise                           = (not . fst $ toElim V.! elimsIdx, elimsIdx)
                                            -- The given input is not specifically listed, 
                                            -- so check to see if we're in a range (fst tup == False)
                                            -- Also, the seqIdx must be lower than the elimsIdx
                                            -- we're not in a range, and the seq idx is not spec'ed
                                            -- we are in a range, so seqIdx should be eliminated, but we don't want to move to the next elim

-- | sortElimsStr takes a String of digits, commas and hyphens and returns a list of strings, broken on the commas
-- and sorted by the value of the Int representation of any numbers before a hyphen. The input and output of this fn are
-- described in the Haddock comments for createElimTups
sortElimsStr :: String -> [String]
sortElimsStr start = 
    result
        where 
            unsortedList = splitOn "," start
            result       = sortBy (comparing pert) unsortedList
            pert x       = read (dropWhile isSpace $ takeWhile (/= '-') x) :: Int

-- | setIgnores iterates from x to y (ints), determining at each index whether that index should be "ignored". If so, it
-- flags True, otherwise it flags False. It accumulates all of these Booleans into a list
setIgnores :: Int -> Int -> V.Vector (Bool,Int) -> Int -> [Bool] -> [Bool]
setIgnores seqIdx curElimIdx toElim seqLength acc
    | seqIdx >= seqLength = reverse acc
    | otherwise           = setIgnores (succ seqIdx) next toElim seqLength (val : acc)
        where
            (val, next) = getNext seqIdx curElimIdx toElim


----------------------------------------------
-- Helper fns
----------------------------------------------

-- It's super inefficient!!!
-- Maybe use `Text`?

lstrip :: String -> String
lstrip "" = ""
lstrip input = dropWhile (`elem` " \t\n\r") input

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
