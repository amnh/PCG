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

module PCG.Command.Types.Report.TaxonMatrix where

import Bio.Phylogeny.Graph
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node hiding (name)
--import PCG.Command.Types.Report.CharacterMatrix

--import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
import Data.List
import Data.Matrix.NotStupid hiding (trace)
import Data.Vector (ifoldr, ifilter, Vector)
import qualified Data.Vector as V
import Data.Maybe

--import Debug.Trace

type Presence = Matrix Bool
type TaxaPresence = (Presence, [String], [String])

taxonReferenceOutput :: Graph -> [FilePath] -> String
--taxonReferenceOutput (Graph dags) _ | trace ("taxon reference output " ++ show (length dags)) False = undefined
taxonReferenceOutput (Graph dags) filterFiles = printIt dags $ combineTaxa $ map makeRef dags
    where
        combineTaxa :: [TaxaPresence] -> TaxaPresence
        --combineTaxa inPres | trace ("combining taxa") False = undefined
        combineTaxa inPres
            | null inPres = (matrix 0 0 (\_ -> False), mempty, mempty)
            | otherwise = foldr1 combineTwo inPres
            where
                -- | This assumes that the same file is not in two different dags
                combineTwo :: TaxaPresence -> TaxaPresence -> TaxaPresence
                combineTwo (mat1, taxa1, files1) (mat2, taxa2, files2) 
                    | taxa1 `checkElems` taxa2 = (mat1 <|> reshuffled2, taxa1, files1 ++ files2)
                    | otherwise = error "Non-overlapping taxa names in forest"
                    where
                        reshuffled2 = matrix (nrows mat2) (ncols mat2) (\(i, j) -> (getCorresponding i) V.! j)
                        getCorresponding index = getRow (fromJust (elemIndex (taxa2 !! index) taxa1)) mat1

                        checkElems :: Eq a => [a] -> [a] -> Bool
                        checkElems x y = null (x \\ y) && null (y \\ x) 

                        --grabAt curName = getRow (fromJust (elemIndex curName names2)) mat2
                        --getIndex curName = fromJust $ elemIndex curName names1
                        --combineSingle curName acc = mapRow (\i val -> (grabAt curName) V.! i || val) (getIndex curName) acc

        makeRef :: DAG -> TaxaPresence
        makeRef inDAG 
            | null $ nodes inDAG = (matrix (length $ getTaxaNames inDAG) (length $ getFileNames inDAG) (\_ -> False) , getTaxaNames inDAG, getFileNames inDAG)
            | otherwise = (V.foldr1 ((<->)) $ V.map (rowVector . filterRow . oneRow) (nodes inDAG), getTaxaNames inDAG, filteredNames)
            where
                files = getFileNames inDAG
                internalPos = ifoldr (\i f acc -> if isPrefixOf "HTU" f then i : acc else acc) mempty (V.fromList files)
                selectPos = foldr (\f acc -> if f `elem` files then fromJust (elemIndex f files) : acc else acc) mempty filterFiles
                filterGiven = ifilter (\i _val -> i `elem` selectPos && not (i `elem` internalPos))
                filterNotGiven = ifilter (\i _val -> not (i `elem` internalPos))
                filterRow = if null filterFiles then filterGiven else filterNotGiven
                filteredNames = if null filterFiles then filter (not . isPrefixOf "HTU") files
                                    else filter (\f -> f `elem` filterFiles && not (isPrefixOf "HTU" f)) files
                --internalPos = ifoldr (\i f acc -> if not isPrefixOf "HTU" f then i : acc else acc) (V.toList files)
                --filterPos = if null filterFiles then [0..length files] 
                --            else foldr (\f acc -> if f `elem` files then fromJust (elemIndex f files) : acc else acc) mempty filterFiles
                --filterRow inRow = ifilter (\i val -> i `elem` filterPos) inRow
                --filteredNames = if null filterFiles then files
                --                    else filter (flip elem filterFiles) files

                oneRow :: NodeInfo -> Vector Bool
                oneRow curNode = V.map (not . isNothing) (encoded curNode)

                getTaxaNames :: DAG -> [String]
                getTaxaNames = IM.fold (:) mempty . nodeNames

                getFileNames :: DAG -> [String]
                --getFileNames inDAG | trace ("getFileNames " ++ show (characters inDAG)) False = undefined
                getFileNames = V.toList . V.map (\c -> fst $ span (/=':') (name c)) . characters


        printIt :: [DAG] -> TaxaPresence -> String
        --printIt dags (mat, taxa, files) | trace ("printing " ++ show (length taxa) ++ " " ++ show (nrows mat)) False = undefined
        printIt _dags (mat, taxa, files) = header ++ ifoldr (\i taxaName acc -> acc ++ "\n" ++ taxaName ++ printRow (getRow i mat)) mempty (V.fromList taxa)
            where
                header = foldr (\fileName acc -> acc ++ ", " ++ fileName) "Taxa" files
                printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty

