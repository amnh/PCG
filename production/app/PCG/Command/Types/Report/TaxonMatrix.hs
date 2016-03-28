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
import PCG.Command.Types.Report.CharacterMatrix

import qualified Data.HashMap as HM
import qualified Data.IntMap as IM
import Data.Matrix
import Data.Vector (ifoldr, imap)
import qualified Data.Vector as V
import Data.Maybe

type Presence = Matrix Bool

taxonReferenceOutput :: Graph -> [FilePath] -> String
taxonReferenceOutput (Graph dags) filterFiles = combineHeader header (foldr ((++) makeRef) mempty dags)
    where
        header = getAllFiles dags

        makeRef :: DAG -> Presence
        makeRef inDAG = V.foldr oneRow mempty (nodes inDAG)

                oneRow :: NodeInfo -> Presence -> [Bool] 
                oneRow curNode accMat = accMat <-> ifoldr (\i c acc -> acc : isNothing c) mempty curSeq

                addName :: Int -> [String] -> [String]
                addName pos curStr = (nodeNames inDAG) IM.! i : curStr

        -- | We make the important assumption that the files represented in one DAG are the same as those in all dags
        -- this helpfully throws an error if this is not the case
        getAllFiles :: [DAG] -> [String]
        getAllFiles (d : ds) 
            | checkAll = getOneDAG d
            | otherwise = error "Not all DAGs have the same file names"
            where
                getOneDAG :: DAG -> [String]  
                getOneDAG inDAG = map (\c -> snd $ span (/=':') (name c)) (HM.values $ characters inDAG) 

                checkAll = foldr (\cd acc -> if getOneDAG cd `checkElems` getOneDAG d then False else True) True ds
                checkElems x y = null (x \\ y) && null (y \\ x) 

        combineHeader :: [String] -> Presence -> String
        combineHeader fileNames presences = undefined