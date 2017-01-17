#include "/Library/Frameworks/GHC.framework/Versions/7.10.3-x86_64/usr/lib/ghc-7.10.3/template-hsc.h"
#line 27 "myComplexBitArrayFFI.hsc"
#include "myComplexBitArrayTestC.h"
#line 28 "myComplexBitArrayFFI.hsc"
#include <stdint.h>

int main (int argc, char *argv [])
{
    hsc_line (1, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("-----------------------------------------------------------------------------\n"
           "", hsc_stdout());
    hsc_line (2, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("-- |\n"
           "-- a more complex example of an FFI interface, for learning\n"
           "--\n"
           "-- This example uses pointers, both to structs and to fields within the \n"
           "-- structs. This is much easier to accomplish via .hsc rather than doing \n"
           "-- straight FFI. A .hsc file are read by hsc2hs, which then creates a .c\n"
           "-- file, which is compiled and run to create an .hs file, which is then \n"
           "-- compiled for use in outside modules.\n"
           "--\n"
           "-----------------------------------------------------------------------------\n"
           "\n"
           "{-# LANGUAGE ForeignFunctionInterface, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}\n"
           "\n"
           "import Bio.Character.Dynamic.Coded.Internal\n"
           "import Data.BitMatrix.Internal\n"
           "import Data.Bits\n"
           "import Data.Foldable\n"
           "import Data.Monoid\n"
           "import Foreign\n"
           "import Foreign.Ptr\n"
           "import Foreign.C.String\n"
           "import Foreign.C.Types\n"
           "import System.IO.Unsafe\n"
           "import Test.QuickCheck hiding ((.&.))\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (28, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (29, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("\n"
           "-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string\n"
           "-- in it, so Ptr CChar.\n"
           "-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures\n"
           "data AlignResult = AlignResult { alignmentCost :: CInt\n"
           "                               , lengthFinal   :: CInt\n"
           "                               , seqFinal      :: Ptr CArrayUnit\n"
           "                               }\n"
           "\n"
           "data CDynamicChar = CDynamicChar { alphabetSize :: CInt\n"
           "                                 , dynCharLen   :: CInt\n"
           "                                 , dynChar      :: Ptr CArrayUnit\n"
           "                                 }\n"
           "\n"
           "instance Show CDynamicChar where\n"
           "    show (CDynamicChar alphSize dcLen dChar) = mconcat [\"alphabetSize:  \"\n"
           "                                                       , show intAlphSize\n"
           "                                                       , \"\\ndynCharLen: \"\n"
           "                                                       , show intLen\n"
           "                                                       , \"\\nbuffer length: \"\n"
           "                                                       , show bufferLength\n"
           "                                                       , \"\\ndynChar:    \"\n"
           "                                                       , show $ unsafePerformIO printedArr\n"
           "                                                       ]\n"
           "        where\n"
           "            (quot, rem)  = (intAlphSize * intLen) `divMod` 64\n"
           "            bufferLength = quot + if rem == 0 then 0 else 1\n"
           "            intAlphSize  = fromIntegral alphSize\n"
           "            intLen       = fromIntegral dcLen\n"
           "            printedArr   = show <$> peekArray bufferLength dChar\n"
           "\n"
           "instance Arbitrary CDynamicChar where\n"
           "    arbitrary = do\n"
           "        alphSize <- (arbitrary :: Gen Int) `suchThat` (\\x -> 0 < x && x <= 64)\n"
           "        charSize <- (arbitrary :: Gen Int) `suchThat` (\\x -> 0 < x && x <= 16)\n"
           "        let (full,rem) = (alphSize * charSize) `divMod` 64\n"
           "        fullBitVals <- vectorOf full (arbitrary :: Gen CArrayUnit)\n"
           "        -- Note there is a faster way to do this loop in 2 steps by utilizing 2s compliment subtraction and setbit.\n"
           "        let mask    = foldl\' (\\val i -> val `setBit` i) (zeroBits :: CArrayUnit) [0..rem]\n"
           "        remBitVals  <- if   rem == 0\n"
           "                       then pure []\n"
           "                       else (pure . (mask .&.)) <$> (arbitrary :: Gen CArrayUnit)\n"
           "        pure CDynamicChar\n"
           "           { alphabetSize = fromIntegral alphSize\n"
           "           , dynCharLen   = fromIntegral charSize\n"
           "           , dynChar      = unsafePerformIO . newArray $ fullBitVals <> remBitVals\n"
           "           }\n"
           "\n"
           "type CArrayUnit  = CULong -- This will be compatible with uint64_t\n"
           "\n"
           "instance Arbitrary CArrayUnit where\n"
           "    arbitrary = do\n"
           "        num <- arbitrary :: Gen Integer\n"
           "        pure $ fromIntegral num\n"
           "\n"
           "instance Storable CDynamicChar where\n"
           "    sizeOf    _ = (", hsc_stdout());
#line 85 "myComplexBitArrayFFI.hsc"
    hsc_size (struct dynChar_t);
    hsc_fputs (") -- #size is a built-in that works with arrays, as are #peek and #poke, below\n"
           "", hsc_stdout());
    hsc_line (86, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("    alignment _ = alignment (undefined :: CArrayUnit)\n"
           "    peek ptr    = do -- to get values from the C app\n"
           "        alphLen  <- (", hsc_stdout());
#line 88 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct dynChar_t, alphSize  );
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (89, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        seqLen   <- (", hsc_stdout());
#line 89 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct dynChar_t, dynCharLen);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (90, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        sequence <- (", hsc_stdout());
#line 90 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct dynChar_t, dynChar   );
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (91, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        return  CDynamicChar { alphabetSize = alphLen\n"
           "                             , dynCharLen   = seqLen\n"
           "                             , dynChar      = sequence  \n"
           "                             }\n"
           "    poke ptr (CDynamicChar alphabetSize dynCharLen dynChar) = do -- to modify values in the C app\n"
           "        (", hsc_stdout());
#line 96 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct dynChar_t, alphSize  );
    hsc_fputs (") ptr alphabetSize\n"
           "", hsc_stdout());
    hsc_line (97, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 97 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct dynChar_t, dynCharLen);
    hsc_fputs (") ptr dynCharLen\n"
           "", hsc_stdout());
    hsc_line (98, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 98 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct dynChar_t, dynChar   );
    hsc_fputs (") ptr dynChar\n"
           "", hsc_stdout());
    hsc_line (99, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("\n"
           "-- Because we\'re using a struct we need to make a Storable instance\n"
           "instance Storable AlignResult where\n"
           "    sizeOf    _ = (", hsc_stdout());
#line 102 "myComplexBitArrayFFI.hsc"
    hsc_size (struct alignResult_t);
    hsc_fputs (") -- #size is a built-in that works with arrays, as are #peek and #poke, below\n"
           "", hsc_stdout());
    hsc_line (103, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("    alignment _ = alignment (undefined :: CArrayUnit)\n"
           "    peek ptr    = do -- to get values from the C app\n"
           "        value    <- (", hsc_stdout());
#line 105 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct alignResult_t, finalWt );
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (106, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        len      <- (", hsc_stdout());
#line 106 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct alignResult_t, finalLength);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (107, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        sequence <- (", hsc_stdout());
#line 107 "myComplexBitArrayFFI.hsc"
    hsc_peek (struct alignResult_t, finalStr);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (108, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        return  AlignResult { alignmentCost = value, lengthFinal = len, seqFinal = sequence }\n"
           "\n"
           "------------- Don\'t need this part, but left in for completion ---------------\n"
           "----- Will get compiler warning if left out, because of missing instances ----\n"
           "    poke ptr (AlignResult cost len seqFinal) = do -- to modify values in the C app\n"
           "        (", hsc_stdout());
#line 113 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct alignResult_t, finalWt);
    hsc_fputs (") ptr cost\n"
           "", hsc_stdout());
    hsc_line (114, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 114 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct alignResult_t, finalLength);
    hsc_fputs (") ptr len\n"
           "", hsc_stdout());
    hsc_line (115, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 115 "myComplexBitArrayFFI.hsc"
    hsc_poke (struct alignResult_t, finalStr);
    hsc_fputs (") ptr seqFinal\n"
           "", hsc_stdout());
    hsc_line (116, "myComplexBitArrayFFI.hsc");
    hsc_fputs ("\n"
           "-- This is the declaration of the Haskell wrapper for the C function we\'re calling.\n"
           "-- Note that this fn is called from testFn.\n"
           "foreign import ccall unsafe \"myComplexBitArrayTestC.h testFn\"\n"
           "    callExtFn_c :: Ptr CDynamicChar -> Ptr CDynamicChar -> Ptr AlignResult -> CInt\n"
           "\n"
           "-- testFn can be called from within Haskell code.\n"
           "testFn :: CDynamicChar -> CDynamicChar -> Either String (Int, String) \n"
           "testFn char1 char2 = unsafePerformIO $ \n"
           "    -- have to allocate memory. Note that we\'re allocating via a lambda fn. In use, the lambda will take whatever is the \n"
           "    -- argument of testFn, but here there is no argument, so all allocation is hard-coded.\n"
           "    alloca $ \\alignPtr -> do \n"
           "        marshalledChar1 <- new char1\n"
           "        marshalledChar2 <- new char2\n"
           "        print marshalledChar1\n"
           "        -- Using strict here because the values need to be read before freeing, \n"
           "        -- so lazy is dangerous.\n"
           "        let !status = callExtFn_c marshalledChar1 marshalledChar2 alignPtr\n"
           "\n"
           "        -- Now checking return status. If 0, then all is well, otherwise throw an error.\n"
           "        if (fromIntegral status) == 0 \n"
           "            then do\n"
           "                AlignResult cost len seq <- peek alignPtr\n"
           "                --seqFinalPtr              <- peek seq\n"
           "                seqFinal                 <- peekArray (fromIntegral len) seq\n"
           "                free seq\n"
           "                pure $ Right (fromIntegral cost, show seqFinal)\n"
           "            else do\n"
           "                pure $ Left \"Out of memory\"\n"
           "        \n"
           "-- Just for testing from CLI outside of ghci.\n"
           "main :: IO ()\n"
           "main = do \n"
           "    char1 <- generate (arbitrary :: Gen CDynamicChar)\n"
           "    --char2 <- generate (arbitrary :: Gen CDynamicChar)\n"
           "    print char1\n"
           "    --print char2\n"
           "    print $ testFn char1 char1\n"
           "\n"
           "\n"
           "data ExportableDynamicCharacter\n"
           "   = ExportableDynamicCharacter\n"
           "   { characterLength :: Int\n"
           "   , alphabetLength  :: Int\n"
           "   , bufferChunks    :: [CArrayUnit]\n"
           "   } deriving (Eq, Show)\n"
           "\n"
           "toExportable :: EncodableDynamicCharacter c => c -> ExportableDynamicCharacter\n"
           "toExportable (DC (BitMatrix alphabetSize bv)) =\n"
           "     ExportableDynamicCharacter\n"
           "     { characterLength = 0\n"
           "     , alphabetLength  = 0\n"
           "     , bufferChunks    = []\n"
           "     }\n"
           "", hsc_stdout());
    return 0;
}
