-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.TaxonMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a matrix stating which taxa are present in which files.
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PCG.Command.Types.Report.TaxonMatrix where

import Bio.Phylogeny.Solution
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node hiding (name)
import qualified Bio.Phylogeny.Tree.Node as N
--import PCG.Command.Types.Report.CharacterMatrix

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Control.Arrow ((***))
import Data.Foldable
import Data.Key
import Data.List
import Data.Matrix.NotStupid hiding (trace, (!), toList)
import Data.Ord    (comparing)
import Data.Vector (ifoldr, ifilter, Vector, cons, imap)
import qualified Data.Vector as V
import Data.Maybe

import Debug.Trace

type Presence = Matrix Bool
type TaxaPresence = (Presence, [String], [String])

instance Monoid Presence where
    mempty = matrix 0 0 (const False)
    mappend = (<|>)

taxonReferenceOutput :: StandardSolution -> [FilePath] -> String
--taxonReferenceOutput sol files | trace (show sol) False = undefined
taxonReferenceOutput sol files = printIt $ makeRef sol files
    where
        makeRef :: StandardSolution -> [FilePath] -> TaxaPresence
        makeRef inSolution inFilter = (presenceMatrix, toList allNodes, toList finalFiles)
                                    --(V.foldr (\n acc -> acc <-> rowVector (oneRow n)) mempty (V.fromList allNodes), allNodes, files)
            where
                presenceMatrix = matrix (length allNodes) (length checkPos) gen
                gen :: (Int,Int) -> Bool
                gen (i,j) = isJust $ (allSeqs ! (allNodes V.! i)) V.! (checkPos V.! j)
                  
                filterNames name = null inFilter || name `elem` inFilter
                fileNames = fst . span (/=':') . name <$> metadata inSolution
                filt@(checkPos, finalFiles) = (V.fromList *** V.fromList)
                                            . unzip
                                            . nubBy (\x y -> snd x == snd y)
                                            . toList
                                            $ V.ifoldr (\i n acc -> if filterNames n then (i, n) `cons` acc else acc) mempty fileNames
                allSeqs = parsedChars inSolution
                allNodes = V.fromList $ HM.keys allSeqs
                oneRow curNode = ifoldr (\i s acc -> if i `elem` checkPos then (not (isNothing s)) `cons` acc else acc) mempty (allSeqs ! curNode)

--taxonReferenceOutput :: StandardSolution -> [FilePath] -> String
--taxonReferenceOutput sol files = printIt $ makeRef sol files
--    where
--        makeRef :: StandardSolution -> [FilePath] -> TaxaPresence
--        makeRef inSolution inFilter = foldr (\forest acc -> combineTwo acc (foldr (\dag acc -> combineTwo acc (oneRef dag filt)) mempty forest)) mempty (forests inSolution)
--            where
--                filterNames name = if null inFilter then True
--                                    else name `elem` inFilter
--                fileNames = fmap (fst . span (/=':') . name) (metadata inSolution)
--                filt@(checkPos, finalFiles) = unzip $ V.toList $ V.ifoldr (\i n acc -> if filterNames n then (i, n) `cons` acc else acc) mempty fileNames

--        oneRef :: DAG -> ([Int], [FilePath]) -> TaxaPresence
--        oneRef inDAG (actualPos, files)
--            | null allNodes = mempty
--            | otherwise = (V.foldr (\n acc -> acc <-> rowVector (oneRow n)) mempty allNodes, nodeNames, files)
--                where
--                    allNodes = nodes inDAG
--                    oneRow curNode = ifoldr (\i s acc -> if i `elem` actualPos then (not (isNothing s)) `cons` acc else acc) mempty (encoded curNode)
--                    nodeNames = foldr (\n acc -> N.name n : acc) mempty allNodes

printIt :: TaxaPresence -> String
printIt (mat, taxa, files) = header ++ ifoldr (\i taxaName acc -> acc ++ "\n" ++ taxaName ++ printRow (getRow i mat)) mempty (V.fromList taxa)
    where
        header = foldr (\fileName acc -> acc ++ ", " ++ fileName) "Taxa" files
        printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty


--combineTwo :: TaxaPresence -> TaxaPresence -> TaxaPresence
--combineTwo (mat1, taxa1, files1) (mat2, taxa2, files2) = undefined
--    where
--        commonTaxa = intersect taxa1 taxa2
--        oneOnly = taxa1 \\ taxa2
--        twoOnly = taxa2 \\ taxa1
--        getCorresponding fromName fromTaxa toTaxa toMatrix = getRow (fromJust $ elemIndex fromName toTaxa) toMatrix
--        intoOne name = getCorresponding name taxa1 taxa2 mat1
--        reshuffled2 = matrix (length commonTaxa) (length files1) (\(i, j) -> intoOne (commonTaxa !! i) V.! j)

--taxonReferenceOutput :: Graph -> [FilePath] -> String
----taxonReferenceOutput (Graph dags) _ | trace ("taxon reference output " ++ show (length dags)) False = undefined
--taxonReferenceOutput (Graph dags) filterFiles = printIt dags $ combineTaxa $ map makeRef dags
--    where
--        combineTaxa :: [TaxaPresence] -> TaxaPresence
--        --combineTaxa inPres | trace ("combining taxa") False = undefined
--        combineTaxa inPres
--            | null inPres = (matrix 0 0 (\_ -> False), mempty, mempty)
--            | otherwise = foldr1 combineTwo inPres
--            where
--                -- | This assumes that the same file is not in two different dags
--                combineTwo :: TaxaPresence -> TaxaPresence -> TaxaPresence
--                combineTwo (mat1, taxa1, files1) (mat2, taxa2, files2) 
--                    | taxa1 `checkElems` taxa2 = (mat1 <|> reshuffled2, taxa1, files1 ++ files2)
--                    | otherwise = error "Non-overlapping taxa names in forest"
--                    where
--                        reshuffled2 = matrix (nrows mat2) (ncols mat2) (\(i, j) -> (getCorresponding i) V.! j)
--                        getCorresponding index = getRow (fromJust (elemIndex (taxa2 !! index) taxa1)) mat1

--                        checkElems :: Eq a => [a] -> [a] -> Bool
--                        checkElems x y = null (x \\ y) && null (y \\ x) 

--                        --grabAt curName = getRow (fromJust (elemIndex curName names2)) mat2
--                        --getIndex curName = fromJust $ elemIndex curName names1
--                        --combineSingle curName acc = mapRow (\i val -> (grabAt curName) V.! i || val) (getIndex curName) acc

--        makeRef :: DAG -> TaxaPresence
--        makeRef inDAG 
--            | null $ nodes inDAG = (matrix (length $ getTaxaNames inDAG) (length $ getFileNames inDAG) (\_ -> False) , getTaxaNames inDAG, getFileNames inDAG)
--            | otherwise = (V.foldr1 ((<->)) $ V.map (rowVector . filterRow . oneRow) (nodes inDAG), getTaxaNames inDAG, filteredNames)
--            where
--                files = getFileNames inDAG
--                internalPos = ifoldr (\i f acc -> if isPrefixOf "HTU" f then i : acc else acc) mempty (V.fromList files)
--                selectPos = foldr (\f acc -> if f `elem` files then fromJust (elemIndex f files) : acc else acc) mempty filterFiles
--                filterGiven = ifilter (\i _val -> i `elem` selectPos && not (i `elem` internalPos))
--                filterNotGiven = ifilter (\i _val -> not (i `elem` internalPos))
--                filterRow = if null filterFiles then filterGiven else filterNotGiven
--                filteredNames = if null filterFiles then filter (not . isPrefixOf "HTU") files
--                                    else filter (\f -> f `elem` filterFiles && not (isPrefixOf "HTU" f)) files
--                --internalPos = ifoldr (\i f acc -> if not isPrefixOf "HTU" f then i : acc else acc) (V.toList files)
--                --filterPos = if null filterFiles then [0..length files] 
--                --            else foldr (\f acc -> if f `elem` files then fromJust (elemIndex f files) : acc else acc) mempty filterFiles
--                --filterRow inRow = ifilter (\i val -> i `elem` filterPos) inRow
--                --filteredNames = if null filterFiles then files
--                --                    else filter (flip elem filterFiles) files

--                oneRow :: NodeInfo -> Vector Bool
--                oneRow curNode = V.map (not . isNothing) (encoded curNode)

--                getTaxaNames :: DAG -> [String]
--                getTaxaNames = IM.fold (:) mempty . nodeNames

--                getFileNames :: DAG -> [String]
--                --getFileNames inDAG | trace ("getFileNames " ++ show (characters inDAG)) False = undefined
--                getFileNames = V.toList . V.map (\c -> fst $ span (/=':') (name c)) . characters


--        printIt :: [DAG] -> TaxaPresence -> String
--        --printIt dags (mat, taxa, files) | trace ("printing " ++ show (length taxa) ++ " " ++ show (nrows mat)) False = undefined
--        printIt _dags (mat, taxa, files) = header ++ ifoldr (\i taxaName acc -> acc ++ "\n" ++ taxaName ++ printRow (getRow i mat)) mempty (V.fromList taxa)
--            where
--                header = foldr (\fileName acc -> acc ++ ", " ++ fileName) "Taxa" files
--                printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty

