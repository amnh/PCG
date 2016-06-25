{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Test.Custom.Types where

--import           Bio.Character.Dynamic.Coded
--import           Bio.PhyloGraph.DAG.Class
--import qualified Bio.PhyloGraph.Network                           as N
--import qualified Bio.PhyloGraph.Node.Encoded                      as EN
--import qualified Bio.PhyloGraph.Node.Final                        as FN
--import qualified Bio.PhyloGraph.Node.ImpliedAlign                 as IN
--import qualified Bio.PhyloGraph.Node.Preliminary                  as RN
--import           Bio.PhyloGraph.Node.Referential
--import           Bio.PhyloGraph.Tree.Binary
--import qualified Bio.PhyloGraph.Tree.Referential                  as RT
--import           Bio.PhyloGraph.Tree.Rose
import           Data.Char
--import           Data.BitVector                   hiding (not)
--import           Data.Key                                (lookup)
--import           Safe
--import           Data.Vector                             (Vector)
--import qualified Data.Vector                                      as V
--import           Test.Custom.Tree 
import           Test.QuickCheck

import           Prelude                          hiding (lookup)

newtype AsciiAlphaNum    = AsciiAlphaNum    { getAsciiAlphaNum    :: Char   } deriving (Eq)
newtype AsciiNonAlphaNum = AsciiNonAlphaNum { getAsciiNonAlphaNum :: Char   } deriving (Eq)
newtype Whitespace       = Whitespace       { getWhitespaceChar   :: Char   } deriving (Eq)
newtype InlineSpace      = InlineSpace      { getInlineSpaceChar  :: Char   } deriving (Eq)
-- | A 'TextToken' consists of one or more printable non-space characters
newtype TextToken        = TextToken        { getTextToken        :: String } deriving (Eq)
-- | A 'WordToken' consists of one or more alphabetic characters
newtype WordToken        = WordToken        { getWordToken        :: String } deriving (Eq)

instance Arbitrary TextToken where
  arbitrary = fmap TextToken . listOf1 $ elements textChar
    where
      textChar = filter (\x -> not (isControl x) && not (isSpace x)) $ chr <$> [0..128]

instance Show TextToken where
  show (TextToken s) = s

instance Arbitrary WordToken where
  arbitrary = fmap WordToken . listOf1 $ elements wordChar
    where
      wordChar = filter isAlpha $ chr <$> [0..128]

instance Show WordToken where
  show (WordToken s) = s

instance Arbitrary AsciiAlphaNum where
  arbitrary = elements nonSpaceChars
    where
      nonSpaceChars = fmap AsciiAlphaNum . filter isAlphaNum  $ chr <$> [0..128]

instance Show AsciiAlphaNum where
  show (AsciiAlphaNum c) = show c

instance Arbitrary AsciiNonAlphaNum where
  arbitrary = elements nonAlphaNumChars
    where
      nonAlphaNumChars = fmap AsciiNonAlphaNum . filter (not . isAlphaNum) $ chr <$> [0..128]

instance Show AsciiNonAlphaNum where
  show (AsciiNonAlphaNum c) = show c

instance Arbitrary Whitespace where
  arbitrary = elements whitespaceChars
    where
      whitespaceChars = Whitespace <$> " \t\n\r\f\v"

instance Show Whitespace where
  show (Whitespace c) = show c

instance Arbitrary InlineSpace where
  arbitrary = elements inlineSpaceChars
    where
      inlineSpaceChars = InlineSpace <$> " \t"

instance Show InlineSpace where
    show (InlineSpace c) = show c
{-
-- Node type for a simple tree for testing
data TestNode = TestNode  { code        :: Int
                          , isRoot      :: Bool
                          , isLeaf      :: Bool
                          , parents     :: [Int]
                          , children    :: [Int]
                          , encoded     :: Vector DynamicChar
                          , preliminary :: Vector DynamicChar
                          , aligned     :: Vector DynamicChar
                          , temporary   :: Vector DynamicChar
                          , final       :: Vector DynamicChar
                          , gapped      :: Vector DynamicChar
                          , iaHomology  :: IN.HomologyTrace
                          , localCost   :: Double
                          , totalCost   :: Double
                          } deriving (Eq, Show)

instance EN.EncodedNode TestNode DynamicChar where
    getEncoded = encoded
    setEncoded n s = n { encoded = s }

instance FN.FinalNode TestNode DynamicChar where
    getFinal           = final
    setFinal f n       = n { final  = f }
    getFinalGapped     = gapped
    setFinalGapped f n = n { gapped = f }

instance RN.PreliminaryNode TestNode DynamicChar where
    getPreliminary      = preliminary
    setPreliminary s n  = n {preliminary = s}
    getPreliminaryAlign = aligned
    setAlign s n        = n {aligned = s}
    getTemporary        = temporary
    setTemporary s n    = n {temporary = s}
    getLocalCost        = localCost
    setLocalCost c n    = n {localCost = c}
    getTotalCost        = totalCost
    setTotalCost c n    = n {totalCost = c}

instance IN.IANode TestNode where
  getHomologies = iaHomology
  setHomologies n h = n {iaHomology = h}

type TestDAG = Vector TestNode

instance N.Network TestDAG TestNode where
  parents n t = fmap (\i -> t V.! i) (parents n)
  root t      = let roots = V.filter isRoot t
                in if V.null roots then error "Binary test tree has no root"
                  else V.head roots
  children n t = fmap (\i -> t V.! i) (children n)
  nodeIsLeaf n _ = isLeaf n
  nodeIsRoot n _ = isRoot n
  update t new = t V.// fmap (\n -> (code n, n)) new
  numNodes = V.length
  addNode t n = t V.++ pure (n {code = V.length t})

instance RT.ReferentialTree TestDAG TestNode where
  getNodeIdx n t = V.elemIndex (getCode n) $ getCode <$> t
  getNthNode     = (V.!)

instance BinaryTree TestDAG TestNode where
    leftChild  n t = lookup 0 $ (\i -> t V.! i) <$> children n
    rightChild n t = lookup 1 $ (\i -> t V.! i) <$> children n
    verifyBinary   = all ((2 >=) . length . children)

instance RoseTree TestDAG TestNode where
    parent n t = headMay $ fmap (\i -> t V.! i) (parents n)

-- TODO: fix this instance, it doesn't make sense, but we need it
instance StandardDAG TestDAG TestNode Int where
  getNodes  = id
  setNodes _ t = t
  getEdges = error "TestDAG does not have edge structure"
  setEdges = error "TestDAG does not have edge structure"
  getRoot = N.root 

instance RefNode TestNode where
  getCode = code
-}
