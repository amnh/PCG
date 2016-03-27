-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.CharacterMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a matrix stating which characters are present in which files.
-- Most importantly, it assumes that all character names have the file name prepended to them.
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module PCG.Command.Types.Report.CharacterMatrix where

import Bio.Phylogeny.Graph
import Bio.Phylogeny.PhyloCharacter
import Data.Matrix.NotStupid ((<->), (<|>), matrix, getElem, setElem, Matrix, getRow)
import Data.Monoid
import           Data.Vector      (Vector, (!), imap, cons)
import qualified Data.Vector as V (elemIndex)

data CharFileMatrix = CFMat {charNames :: Vector String, fileNames :: Vector String, presence :: Matrix Bool}

instance Monoid CharFileMatrix where
    mempty = CFMat mempty mempty (matrix 0 0 (const False))
    mappend (CFMat cn1 fn1 m1) (CFMat cn2 fn2 m2) = CFMat (cn1 <> cn2) (fn1 <> fn2) (m1 <-> m2)

-- | Wrapper function to put a graph in a character matrix
outPutMatrix :: String -> Graph -> IO ()
outPutMatrix fileName inGraph = writeFile fileName (toCharMat inGraph)
    where
        toCharMat :: Graph -> String
        toCharMat (Graph dags) = printMat $ foldr (combineMats . makeMat) mempty dags

        -- | Most important function to make a single matrix to be folded
        makeMat :: DAG -> CharFileMatrix
        makeMat inDAG = 
            let
                charFileNames = imap (\i c -> separateNames i $ name c) (characters inDAG)
                addIt val acc = if val `elem` acc then acc else val `cons` acc
                uniqueChars = foldr (\p acc -> addIt (fst p) acc) mempty charFileNames
                uniqueFiles = foldr (\p acc -> addIt (snd p) acc) mempty charFileNames
                emptyMat = matrix (length uniqueChars) (length uniqueFiles) (const False)
                outMat = foldr (\(char, file) accMat -> setElem True (myIndex char uniqueChars, myIndex file uniqueFiles) accMat) emptyMat charFileNames
            in CFMat uniqueChars uniqueFiles outMat

        -- | custom unsafe indexing
        myIndex :: Eq a => a -> Vector a -> Int
        myIndex val vec = fromMaybe errMsg $ V.elemIndex val vec
          where
            errMsg = error "Problem folding characters into matrix"

        -- | In combining two CharFileMatrices, we assume that there are no common files between DAGS,
        -- but that there might be common characters.
        combineMats :: CharFileMatrix -> CharFileMatrix -> CharFileMatrix
        combineMats (CFMat chars1 files1 presence1) (CFMat chars2 files2 presence2) = 
            let
                commonChars = foldr (\e acc -> if e `elem` chars2 then e `cons` acc else acc) mempty chars2
                complementVecs v1 v2 = foldr (\e acc -> if e `elem` v2 then acc else e `cons` acc) mempty v1
                oneNotTwo = complementVecs chars1 chars2
                twoNotOne = complementVecs chars2 chars1
                allFiles = files1 <> files2
                commonGenerator (cPos, fPos) curChars prevMat prevChars = getElem (myIndex (curChars ! cPos) prevChars) fPos prevMat
                common1 = matrix (length commonChars) (length files1) (\p -> commonGenerator p commonChars presence1 chars1)
                common2 = matrix (length commonChars) (length files2) (\p -> commonGenerator p commonChars presence2 chars2)
                commonMat = common1 <|> common2
                unique1 = matrix (length oneNotTwo) (length files1) (\p -> commonGenerator p oneNotTwo presence1 chars1)
                unique2 = matrix (length twoNotOne) (length files2) (\p -> commonGenerator p twoNotOne presence2 chars2)
                finalMat = commonMat <-> (unique1 <|> unique2)
            in CFMat (commonChars <> oneNotTwo <> twoNotOne) allFiles finalMat

        printMat :: CharFileMatrix -> String
        printMat (CFMat chars files mat) = foldr  (\i acc -> acc ++ printFullRow i) printHeader [0..length chars - 1]
            where
                printHeader = foldr (\e acc -> acc ++ e ++ ", ") mempty files ++ "\n"
                printFullRow index = (chars ! index) ++ ", " ++ printBools (getRow index mat) ++ "\n"
                printBools = foldr (\e acc -> acc ++ (if e then "+" else "-") ++ ", ") mempty

        -- | Separates the file and character names, defaulting character name as needed
        separateNames :: Int -> String -> (String, FilePath)
        separateNames curPos fullName = 
          case span (/=':') fullName of
            (             _,             []) -> (show curPos        , fullName)
            (fileNamePrefix, charNameSuffix) -> (tail charNameSuffix, fileNamePrefix) --tail to remove leading ':'

