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
import Bio.Metadata
import Bio.Phylogeny.Node hiding (name)
import qualified Bio.Phylogeny.Node as N

import qualified Data.HashMap.Strict as HM
--import qualified Data.IntMap as IM
import Control.Arrow ((***))
import Data.Function (on)
import Data.Foldable
import Data.Key
import Data.List
--import Data.List.Utility
import Data.Matrix.NotStupid hiding (trace, (!), toList)
--import Data.Ord    (comparing)
import Data.Vector (cons, ifoldr)
import qualified Data.Vector as V
import Data.Maybe

--import Debug.Trace

type Presence = Matrix Bool
type TaxaPresence = (Presence, [String], [String])

taxonReferenceOutput :: StandardSolution -> [FilePath] -> String
taxonReferenceOutput sol files = printIt $ makeRef sol files
    where
        makeRef :: StandardSolution -> [FilePath] -> TaxaPresence
        makeRef inSolution inFilter = (presenceMatrix, toList allNodes, toList finalFiles)
                                    --(V.foldr (\n acc -> acc <-> rowVector (oneRow n)) mempty (V.fromList allNodes), allNodes, files)
            where
                presenceMatrix = matrix (length allNodes) (length checkPos) gen
                gen :: (Int,Int) -> Bool
                --gen (i,j) | trace ("gen on row " ++ show i ++ " " ++ show (length (allSeqs ! (allNodes V.! i))) ++ " " ++ show (checkPos V.! j)) False = undefined
                gen (i,j) = isJust $ (allSeqs ! (allNodes V.! i)) V.! (checkPos V.! j)
                  
                filterNames taxonName = null inFilter || taxonName `elem` inFilter
                fileNames = takeWhile (/=':') . name <$> metadata inSolution
                (checkPos, finalFiles) = (V.fromList *** V.fromList)
                                       . unzip
                                       . nubBy ((==) `on` snd)
                                       . toList
                                       $ V.ifoldr (\i n acc -> if filterNames n then (i, n) `cons` acc else acc) mempty fileNames
                allSeqs = parsedChars inSolution
                allNodes = V.fromList $ HM.keys allSeqs
--                oneRow curNode = ifoldr (\i s acc -> if i `elem` checkPos then isJust s `cons` acc else acc) mempty (allSeqs ! curNode)

printIt :: TaxaPresence -> String
printIt (mat, taxa, files) = header ++ ifoldr (\i taxaName acc -> acc ++ "\n" ++ taxaName ++ printRow (getRow i mat)) mempty (V.fromList taxa)
    where
        header = foldr (\fileName acc -> acc ++ ", " ++ fileName) "Taxa" files
        printRow = V.foldr (\b acc -> if b then ", +" ++ acc else ", -" ++ acc) mempty

