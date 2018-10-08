{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Constructions where


import           Bio.Character.Encodable
import           Bio.Character.Parsed
import           Data.Alphabet
import           Data.BitMatrix
import           Data.BitVector                      (bitVec)
--import           Data.Foldable
--import qualified Data.List.NonEmpty as NE
import qualified Data.Vector                         as V
import           Test.Custom.Types                   ()
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances ()


-- | Function to generate an arbitrary DynamicCharacter given an alphabet
arbitraryDynamicGivenAlph :: Alphabet String -> Gen DynamicCharacter
arbitraryDynamicGivenAlph inAlph = do
    arbParsed <- arbitrary :: Gen ParsedChar -- TODO: Surely this also needs to depend on the alphabet?
    pure . encodeStream inAlph $ arbParsed


-- | Generate many dynamic characters using the above
arbitraryDynamicsGA :: Alphabet String -> Gen DynamicCharacters
arbitraryDynamicsGA inAlph = V.fromList <$> listOf (arbitraryDynamicGivenAlph inAlph)


-- | An infinite list of (non-empty) 'DynamicCharacter's of fixed width and varying length.
arbitraryDynamicCharacterStream :: Gen [DynamicCharacter]
arbitraryDynamicCharacterStream = do
    alphabetLen  <- arbitrary `suchThat` (\x -> 0 < x && x <= 62) :: Gen Int
    let randChar = do characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
                      let randVal  =  choose (1, 2 ^ alphabetLen - 1) :: Gen Integer
                      bitRows      <- vectorOf characterLen randVal
                      pure . DC . fromRows $ bitVec alphabetLen <$> bitRows
    infiniteListOf randChar

