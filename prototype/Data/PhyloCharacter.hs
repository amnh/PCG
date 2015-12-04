{-# LANGUAGE DeriveGeneric #-}


module Data.PhyloCharacter where

import Data.Vector
import Data.Bits
import qualified ReadFiles as RF
import Data.Int
import GHC.Generics

data PhyloCharacter b = DNA {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String}
                        | RNA {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String}
                        | Morphology {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String}
                        | Continous 
                        | Custom deriving (Show, Eq, Read, Generic)


--class PhyloCharacter a b | a -> b where
--    aligned :: a -> Bool
--    setAligned :: a -> Bool -> a
--    masks :: a -> Maybe (Vector b, Vector b)
--    setMasks :: a -> Maybe (Vector b, Vector b) -> a
--    alphabet :: a -> Vector String
--    setAlphabet :: a -> Vector String -> a
--    charType :: a -> RF.CharType
--    setCharType :: a -> RF.CharType -> a

--instance PhyloCharacter RF.CharInfo Int64 where
--    aligned n = True
--    setAligned n _ = n
--    alphabet n = fromList $ RF.alphabet n
--    setAlphabet n alph = n {RF.alphabet = toList alph}
--    charType = RF.charType
--    setCharType n val = n {RF.charType = val}
--    masks = const Nothing
--    setMasks n _ = n