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

import           Data.Char              (toLower)
import           Data.Either            (lefts)
import           Data.List              (sort)
import qualified Data.Map.Lazy as M
import           Data.Maybe             (fromJust, catMaybes, maybeToList)
import qualified Data.Vector as V
import           File.Format.Nexus.Data
import           Safe
import           Text.Megaparsec.Prim   (MonadParsec)
import           Text.Megaparsec.Custom


-- Because some errors are dependent on passing some other validation, they've been split into
-- dependant and independant errors. Errors are enumerated below, along with their dependancies.
-- Comments are included in the code that explain which of the below errors are caught at various points.
-- Note that nos. 4--6, below, are only dependent on 3, so are simply done as nested ifs. This may
-- or may not be best practice.                                                             coded as
-- Errors currently caught: # error                                       dependency   inde- or de- pendant    finished
--  1. Too many taxa blocks                                                    -              indep              done
--  ** removed ** too many sequence blocks                                     -              indep              done
--     There are actually two subcases here:
--        too many aligned, too many unaligned
--  ** removed ** No sequence blocks                                           -              indep              done
--  2. No usable blocks                                                        -              indep
--  4. No seqMatrix in some sequence block                                     3              indep              done
--  5. No dimensions in some sequence block                                    3              indep              done
--  6. Non-data sequence block with "nolabels", 
--     but no taxa block _and_ no new taxa specified in sequence blocks        3              indep              done
--  7. "newtaxa" keyword in characters or unaligned block, 
--     but no nolables and no taxlabels actually spec'ed                       3                dep              done
--  8. "newtaxa" keywork in characters or unaligned block, but ntaxa missing   3,5              dep              done
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
-- 18. Missing semicolons                                                      -              indep              caught in parse
-- 19. Equate or symbols strings missing their closing quotes                  3                dep              done
-- 20. Match character can't be a space                                        3                dep
--     ---for aligned blocks, will be caught
--     ---as with 13, not sure how to catch for unaligned
-- 21. Space in taxon name                                                     5,6              dep              done
--     ---for aligned, should be caught by 16
--     ---for unaligned, caught by combo of 12 & 22
-- 22. In unaligned, interleaved block, a taxon is repeated                    3                dep              done
validateNexusParseResult :: (Show s, MonadParsec s m Char) => NexusParseResult -> m Nexus
validateNexusParseResult (NexusParseResult sequences taxas _treeSet assumptions _ignored)
  | not (null independentErrors) = fails independentErrors
  | not (null dependentErrors)   = fails dependentErrors
  -- TODO: first arg to Nexus was commented out before first push to Grace
  -- TODO: unalignedTaxaSeqMap was commented out before first push to Grace.
  -- When it's added back, downstream (i.e. Nexus) fns will need to be modified
  -- to expect a *list* of 
  | otherwise                  = pure $ Nexus {-taxaLst-} alignedTaxaSeqMap costMatrix {- : unalignedTaxaSeqMap -}
  where
        alignedTaxaSeqMap = getSeqFromMatrix (getBlock "aligned" sequences) taxaLst
        --TODO: dependentErrors & independentErrors becomes :: String error, String warning => [Maybe (Either error warning)]
        -- then partitionEithers . catMaybes, etc., etc.
        costMatrix = head assumptions
        dependentErrors = catMaybes $ incorrectTaxaCount : (missingCloseQuotes ++ seqTaxaCountErrors ++ interleaveErrors ++ seqTaxonCountErrors ++ incorrectCharCount)
        equates = foldr (\x acc -> getEquates x : acc) [] sequences
        independentErrors = catMaybes $ noTaxaError : multipleTaxaBlocks : sequenceBlockErrors
        f [x] = Just x
        f _   = Nothing

        incorrectCharCount  = checkSeqLength (getBlock "aligned" sequences) alignedTaxaSeqMap
        incorrectTaxaCount  = f taxas >>= \(TaxaSpecification num taxons) -> if num /= length taxons
                then Just $ "Incorrect number of taxa in taxa block.\n" {- ++ (show num) ++ " " ++ (show taxons) -} -- half of error 16
                else Nothing
        interleaveErrors = foldr (\x acc -> findInterleaveError taxaLst x : acc) [] sequences -- error 14
        seqMatrixDimsErrors = foldr (\x acc -> seqMatrixMissing x : acc) [] sequences ++
                           foldr (\x acc -> dimsMissing   x : acc) [] sequences
        missingCloseQuotes = map Just (lefts equates) ++ map Just (lefts symbols') -- error 19
        multipleTaxaBlocks = case taxas of
                            (_:_:_) -> Just "Multiple taxa blocks supplied.\n"  -- error 1
                            _       -> Nothing
        noTaxaError =  if null taxas && not (foldr (\x acc -> acc || areNewTaxa x) False sequences) -- will register as False if sequences is empty
                       then Just $ "Taxa are never specified. \n"  {-++ (show taxas) ++ " " ++ (show sequences) -} -- error 6
                       else Nothing
        seqTaxaCountErrors = foldr (\x acc -> checkForNewTaxa x : acc) [] sequences -- errors 7, 8 half of 16
        seqTaxonCountErrors = foldr (\x acc -> getSeqTaxonCountErrors taxaLst x ++ acc) [] sequences -- errors 12, 22
        
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
                                         | otherwise                    -> seqMatrixDimsErrors
                                 _         -> seqMatrixDimsErrors
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
                                                else Just ("\"" ++ key ++ "\" is in a matrix in a sequence block, but isn't specified anywhere, such as in a taxa block or as newtaxa.\n"))
                                                  : acc
                                 ) [] seqTaxaMap
        wrongCountErrors = M.foldrWithKey (\key val acc -> (if val /= median
                                                                   then Just ("\"" ++ key ++ "\" appears the wrong number of times in a sequence block matrix.\n")
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
        mtx     = head $ seqMatrix seq' -- I've already checked to make sure there's a matrix
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
seqMatrixMissing :: PhyloSequence -> Maybe String
seqMatrixMissing seq'
  | numMatrices < 1 = tooFew
  | numMatrices > 1 = tooMany
  | otherwise       = Nothing
  where
    numMatrices = length $ seqMatrix seq'
    tooFew      = Just $ name seq' ++ " block has no sequence matrix.\n"
    tooMany     = Just $ name seq' ++ " block has more than one sequence matrix.\n"

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
        lineCount   = length . head $ seqMatrix seq'
        which = if aligned seq'
                    then "Characters"
                    else "Unaligned"

-- TODO: Review this function's functionality. Seems like it can be improved!
getBlock :: String -> [PhyloSequence] -> [PhyloSequence]
getBlock _ [] = []
getBlock which phyloSeqs = f (which == "aligned") phyloSeqs
  where
    f xBool _ = let block = headMay $ filter (\s -> xBool == aligned s) phyloSeqs
                in maybeToList block


getFormatInfo :: PhyloSequence -> (Bool, Bool, Bool, Bool, String)
getFormatInfo phyloSeq = case headMay $ format phyloSeq of
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
        taxaMap    = M.fromList . zip taxaLst $ repeat ""
        mtx        = head $ seqMatrix $ head seqLst -- I've already checked to make sure there's a matrix
        entireSeqs = if noLabels    -- this will be a list of tuples (taxon, concatted seq)
                     then if interleaved
                          then concatMap (zip taxaLst) (chunksOf taxaCount mtx)
                          else zip taxaLst mtx
                     else getTaxonAndSeqFromMatrixRow <$> mtx
        entireDeinterleavedSeqs = if interleaved
                                  then deInterleave taxaMap entireSeqs
                                  else M.fromList entireSeqs
        firstSeq = fromJust $ M.lookup (if noLabels
                                        then head taxaLst
                                        else takeWhile (`notElem` " \t") $ head mtx)
                            entireDeinterleavedSeqs
        matchCharsReplaced = if matchChar' /= ""
                             then M.map (replaceMatches (head matchChar') firstSeq) entireDeinterleavedSeqs
                             else entireDeinterleavedSeqs
        -- equatesReplaced = if eqChar /= ""
        --                    then M.map (replaceEquates (head eqChar)) matchCharsReplaced

-- | deInterleaved takes in a Map  interleaved seqMatrix in the form [(taxon,sequence)] and returns
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
        phyloSeq            = dropWhile (`elem` " \t") rest

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = f : chunksOf n s
  where
    (f,s) = splitAt n xs

areNewTaxa :: PhyloSequence -> Bool
areNewTaxa phyloSeq
    | name phyloSeq == "data" = True
    | otherwise          =
      case charDims phyloSeq of
        []  -> False
        xs  -> case seqTaxaLabels phyloSeq of
                [] -> False
                _  -> newTaxa $ head xs

--checkDims :: Phylosequence -> Maybe String
--checkDims seq = let dim = numchars $ head (charDims seq)
--                in if dim == length

-- This is frighteningly similar to areNewTaxa, but I couldn't figure out a way around
-- having them both, because of the way that areNewTaxa is used
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
    which = if aligned phyloSeq
            then "characters"
            else "unaligned"

getTaxaFromSeq :: PhyloSequence -> [String]
getTaxaFromSeq phyloSeq 
    | areNewTaxa phyloSeq = case seqTaxaLabels phyloSeq of
                         []    -> {- trace (show keys) $ -} keys
                         (x:_) -> x
    | otherwise        = []
    where
        keys = M.keys $ getTaxaFromMatrix phyloSeq



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
