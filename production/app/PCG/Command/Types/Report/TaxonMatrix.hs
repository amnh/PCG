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
import Bio.Phylogeny.Tree.Node
import PCG.Command.Types.Report.CharacterMatrix

import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
import Data.List
import Data.Matrix.NotStupid hiding (trace)
import Data.Vector (ifoldr, imap, ifilter, Vector)
import qualified Data.Vector as V
import Data.Maybe

import Debug.Trace

type Presence = Matrix Bool
type TaxaPresence = (Presence, [String], [String])

taxonReferenceOutput :: Graph -> [FilePath] -> String
--taxonReferenceOutput (Graph dags) _ | trace ("taxon reference output " ++ show (length dags)) False = undefined
taxonReferenceOutput (Graph dags) filterFiles = printIt dags $ combineTaxa $ map makeRef dags
    where
        combineTaxa :: [TaxaPresence] -> TaxaPresence
        combineTaxa inPres | trace ("combining taxa") False = undefined
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
            | otherwise = (V.foldr1 ((<->)) $ V.map (rowVector . filterRow . oneRow) (nodes inDAG), getTaxaNames inDAG, files)
            where
                files = getFileNames inDAG
                filterPos = foldr (\f acc -> if f `elem` files then fromJust (elemIndex f files) : acc else acc) mempty filterFiles
                filterRow inRow = ifilter (\i val -> i `elem` filterPos) inRow

                oneRow :: NodeInfo -> Vector Bool
                oneRow curNode = V.map (not . isNothing) (encoded curNode)

                getTaxaNames :: DAG -> [String]
                getTaxaNames inDAG = IM.fold (:) mempty (nodeNames inDAG)

                getFileNames :: DAG -> [String]
                getFileNames inDAG | trace ("getFileNames " ++ show (characters inDAG)) False = undefined
                getFileNames inDAG = filter (flip elem filterFiles) unfiltered
                    where unfiltered = V.toList $ V.map (\c -> fst $ span (/=':') (name c)) (characters inDAG) 


        printIt :: [DAG] -> TaxaPresence -> String
        --printIt dags (mat, taxa, files) | trace ("printing " ++ show (length taxa) ++ " " ++ show (nrows mat)) False = undefined
        printIt dags (mat, taxa, files) = header ++ ifoldr (\i name acc -> acc ++ "\n" ++ name ++ printRow (getRow i mat)) mempty (V.fromList taxa)
            where
                header = foldr (\name acc -> acc ++ ", " ++ name) "Taxa" files
                printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty

