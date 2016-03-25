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
import Data.List
import Data.Matrix.NotStupid ((<->), (<|>), matrix, getElem, setElem, Matrix, getRow)
import Data.Monoid
import Data.Vector (imap, toList, Vector, cons, ifoldr, (!))
import qualified Data.Vector as V (foldr, elem, length, elemIndex, (++))

data CharFileMatrix = CFMat {charNames :: Vector String, fileNames :: Vector String, presence :: Matrix Bool}

instance Monoid (Matrix Bool) where
    mempty = matrix 0 0 (const False)
    mappend = (<->)

instance Monoid CharFileMatrix where
    mempty = CFMat mempty mempty mempty
    mappend (CFMat cn1 fn1 m1) (CFMat cn2 fn2 m2) = CFMat (cn1 <> cn2) (fn1 <> fn2) (m1 <> m2)

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
                addIt val acc = if V.elem val acc then acc else val `cons` acc
                uniqueChars = V.foldr (\p acc -> addIt (fst p) acc) mempty charFileNames
                uniqueFiles = V.foldr (\p acc -> addIt (snd p) acc) mempty charFileNames
                emptyMat = matrix (V.length uniqueChars) (V.length uniqueFiles) (const False)
                outMat = V.foldr (\(char, file) accMat -> setElem True (myIndex char uniqueChars, myIndex file uniqueFiles) accMat) emptyMat charFileNames
            in CFMat uniqueChars uniqueFiles outMat

        -- | custom unsafe indexing
        myIndex :: Eq a => a -> Vector a -> Int
        myIndex val vec = case V.elemIndex val vec of
                                Nothing -> error "Problem folding characters into matrix"
                                Just i -> i

        -- | In combining two CharFileMatrices, we assume that there are no common files between DAGS,
        -- but that there might be common characters.
        combineMats :: CharFileMatrix -> CharFileMatrix -> CharFileMatrix
        combineMats (CFMat chars1 files1 presence1) (CFMat chars2 files2 presence2) = 
            let
                commonChars = V.foldr (\e acc -> if e `V.elem` chars2 then e `cons` acc else acc) mempty chars2
                complementVecs v1 v2 = foldr (\e acc -> if e `V.elem` v2 then acc else e `cons` acc) mempty v1
                oneNotTwo = complementVecs chars1 chars2
                twoNotOne = complementVecs chars2 chars1
                allFiles = files1 V.++ files2
                commonGenerator (cPos, fPos) curChars prevMat prevChars = getElem (myIndex (curChars ! cPos) prevChars) fPos prevMat
                common1 = matrix (V.length commonChars) (V.length files1) (\p -> commonGenerator p commonChars presence1 chars1)
                common2 = matrix (V.length commonChars) (V.length files2) (\p -> commonGenerator p commonChars presence2 chars2)
                commonMat = common1 <|> common2
                unique1 = matrix (V.length oneNotTwo) (V.length files1) (\p -> commonGenerator p oneNotTwo presence1 chars1)
                unique2 = matrix (V.length twoNotOne) (V.length files2) (\p -> commonGenerator p twoNotOne presence2 chars2)
                finalMat = commonMat <-> (unique1 <|> unique2)
            in CFMat (commonChars V.++ oneNotTwo V.++ twoNotOne) allFiles finalMat

        printMat :: CharFileMatrix -> String
        printMat (CFMat chars files presence) = foldr  (\i acc -> acc ++ printFullRow i) printHeader [0..V.length chars - 1]
            where
                printHeader = V.foldr (\e acc -> acc ++ e ++ ", ") mempty files ++ "\n"
                printFullRow index = (chars ! index) ++ ", " ++ (printBools $ getRow index presence) ++ "\n"
                printBools = V.foldr (\e acc -> acc ++ (if e then "+" else "-") ++ ", ") mempty

        -- | Separates the file and character names, defaulting character name as needed
        separateNames :: Int -> String -> (String, String)
        separateNames curPos name = 
            let
                colonPos = findIndex ((==) ':') name
                (fileName, charName) = case colonPos of
                                        Nothing -> error "File name not prepended to character name with colon"
                                        Just p -> splitAt p name
                outChar = if null charName then show curPos else charName
            in (outChar, fileName)
