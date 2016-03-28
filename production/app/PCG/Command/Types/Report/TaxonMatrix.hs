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
import Data.Matrix hiding (trace)
import Data.Vector (ifoldr, imap)
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
                        reshuffled2 = matrix (nrows mat1) (ncols mat2) (\(i, j) -> (getCorresponding i) V.! j)
                        getCorresponding index = getRow (fromJust (elemIndex (taxa1 !! index) taxa2)) mat2
                        --grabAt curName = getRow (fromJust (elemIndex curName names2)) mat2
                        --getIndex curName = fromJust $ elemIndex curName names1
                        --combineSingle curName acc = mapRow (\i val -> (grabAt curName) V.! i || val) (getIndex curName) acc

        makeRef :: DAG -> TaxaPresence
        makeRef inDAG 
            | null $ nodes inDAG = (matrix (length $ getTaxaNames inDAG) (length $ getFileNames inDAG) (\_ -> False) , getTaxaNames inDAG, getFileNames inDAG)
            | otherwise = (V.foldr1 ((<->)) $ V.map oneRow (nodes inDAG), getTaxaNames inDAG, getFileNames inDAG)
            where
                oneRow :: NodeInfo -> Presence
                oneRow curNode = matrix 1 (V.length checkVec) (\(_, i) -> isNothing $ checkVec V.! i)
                    where checkVec = encoded curNode

                getTaxaNames :: DAG -> [String]
                getTaxaNames inDAG = IM.fold (:) mempty (nodeNames inDAG)

                getFileNames :: DAG -> [String]
                getFileNames inDAG = V.toList $ V.map (\c -> snd $ span (/=':') (name c)) (characters inDAG) 

        -- | We make the important assumption that the files represented in one DAG are the same as those in all dags
        -- this helpfully throws an error if this is not the case
        --getAllFiles :: [DAG] -> [String]
        --getAllFiles (d : ds) 
        --    | checkAll = getOneDAG d
        --    | otherwise = error "Not all DAGs have the same file names"
        --    where
        --        getOneDAG :: DAG -> [String]  
        --        getOneDAG inDAG = V.toList $ V.map (\c -> snd $ span (/=':') (name c)) (characters inDAG) 

        --        checkAll = foldr (\cd acc -> if getOneDAG cd `checkElems` getOneDAG d then False else True) True ds

        checkElems :: Eq a => [a] -> [a] -> Bool
        checkElems x y = null (x \\ y) && null (y \\ x) 

        printIt :: [DAG] -> TaxaPresence -> String
        printIt dags (mat, taxa, files) = header ++ ifoldr (\i name acc -> acc ++ "\n" ++ name ++ ", " ++ printRow (getRow i mat)) mempty (V.fromList taxa)
            where
                header = foldr (\name acc -> name ++ ", " ++ acc) mempty files
                printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty



