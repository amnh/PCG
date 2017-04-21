#include "/Library/Frameworks/GHC.framework/Versions/7.10.3-x86_64/usr/lib/ghc-7.10.3/template-hsc.h"
#line 28 "externalCAlignment.hsc"
#include "alignSequences.h"
#line 29 "externalCAlignment.hsc"
#include "nwMatrices.h"

int main (int argc, char *argv [])
{
    hsc_line (1, "externalCAlignment.hsc");
    hsc_fputs ("-----------------------------------------------------------------------------\n"
           "", hsc_stdout());
    hsc_line (2, "externalCAlignment.hsc");
    hsc_fputs ("-- |\n"
           "-- a more complex example of an FFI interface, for learning\n"
           "--\n"
           "-- This example uses pointers, both to structs and to fields within the\n"
           "-- structs. This is much easier to accomplish via .hsc rather than doing\n"
           "-- straight FFI. A .hsc file are read by hsc2hs, which then creates a .c\n"
           "-- file, which is compiled and run to create an .hs file, which is then\n"
           "-- compiled for use in outside modules.\n"
           "--\n"
           "-- For notes on usage, data construction and external see referenced C\n"
           "-- compilation units, and also driver.c, which is not imported, but is\n"
           "-- included indirectory for reference.\n"
           "--\n"
           "-----------------------------------------------------------------------------\n"
           "\n"
           "-- TODO: do I need this: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html\n"
           "\n"
           "{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}\n"
           "\n"
           "import Foreign\n"
           "import Foreign.Ptr\n"
           "import Foreign.C.String\n"
           "import Foreign.C.Types\n"
           "import Foreign.Marshall.Array\n"
           "import System.IO.Unsafe\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (29, "externalCAlignment.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (30, "externalCAlignment.hsc");
    hsc_fputs ("\n"
           "type CArrayUnit  = CULong -- This will be compatible with uint64_t\n"
           "type CDirMtxUnit = CShort -- C short type\n"
           "\n"
           "{- ******************************************* Sequence declaration and Storable instance ******************************************* -}\n"
           "\n"
           "data Alignment2d = Alignment2d { alignedSequence1 :: AlignIO\n"
           "                               , alignedSequence2 :: AlignIO\n"
           "                               , alignment        :: AlignIO\n"
           "                               , cost             :: CInt\n"
           "                               }\n"
           "\n"
           "data Alignment3d = Alignment3d { alignedSequence1 :: AlignIO\n"
           "                               , alignedSequence2 :: AlignIO\n"
           "                               , alignedSequence3 :: AlignIO\n"
           "                               , alignment        :: AlignIO\n"
           "                               , cost             :: CInt\n"
           "                               }\n"
           "\n"
           "\n"
           "data AlignIO = AlignIO { -- magic_number :: CInt     -- TODO: No idea what this is for; figure it out\?\n"
           "                         sequence :: Ptr CInt     -- Capacity of the sequence memory structure.\n"
           "                       , seqLen   :: CSize        -- Total length of the sequence stored.\n"
           "                       }\n"
           "\n"
           "-- Because we\'re using a struct we need to make a Storable instance\n"
           "instance Storable AlignIO where\n"
           "    sizeOf    _  = (", hsc_stdout());
#line 57 "externalCAlignment.hsc"
    hsc_size (struct alignIO);
    hsc_fputs (") -- #size is a built-in that works with arrays, as are #peek and #poke, below\n"
           "", hsc_stdout());
    hsc_line (58, "externalCAlignment.hsc");
    hsc_fputs ("    alignment _  = alignment (undefined :: CChar)\n"
           "    peek ptr     = do                 -- to get values from the C app\n"
           "        len             <- (", hsc_stdout());
#line 60 "externalCAlignment.hsc"
    hsc_peek (struct alignIO, len);
    hsc_fputs (")      ptr\n"
           "", hsc_stdout());
    hsc_line (61, "externalCAlignment.hsc");
    hsc_fputs ("        arr :: Ptr CInt <- (", hsc_stdout());
#line 61 "externalCAlignment.hsc"
    hsc_peek (struct alignIO, sequence);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (62, "externalCAlignment.hsc");
    hsc_fputs ("        seq             <- (", hsc_stdout());
#line 62 "externalCAlignment.hsc"
    hsc_peekArray (len arr);
    hsc_fputs (")             ptr\n"
           "", hsc_stdout());
    hsc_line (63, "externalCAlignment.hsc");
    hsc_fputs ("\n"
           "        return  AlignIO { seqLen   = len\n"
           "                        , sequence = seq\n"
           "                        }\n"
           "    poke ptr (AlignIO len seq ) = do -- to modify values in the C app\n"
           "        (", hsc_stdout());
#line 68 "externalCAlignment.hsc"
    hsc_pokeArray (len (struct alignIO, sequence));
    hsc_fputs (") ptr seq\n"
           "", hsc_stdout());
    hsc_line (69, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 69 "externalCAlignment.hsc"
    hsc_poke (struct alignIO, seqLen);
    hsc_fputs (")              ptr len\n"
           "", hsc_stdout());
    hsc_line (70, "externalCAlignment.hsc");
    hsc_fputs ("\n"
           "\n"
           "{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}\n"
           "-- | Holds single cost matrix, which contains costs and medians for all\n"
           "-- possible sequence elements. It is completely filled using a TCM. See note below at \'setupCostMatrixFn_c\'.\n"
           "data CostMatrix2d = CostMatrix2d { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if\n"
           "                                                              --     combinations == True\n"
           "                                 , lcm           :: CInt      -- ceiling of log_2 (alphSize)\n"
           "                                 , gapChar       :: CInt      -- gap value (1 << (alphSize - 1))\n"
           "                                 , costModelType :: CInt      {- The type of cost model to be used in the alignment,\n"
           "                                                               - i.e. affine or not.\n"
           "                                                               - Based on cost_matrix.ml, values are:\n"
           "                                                               - \342\200\242 linear == 0\n"
           "                                                               - \342\200\242 affine == 1\n"
           "                                                               - \342\200\242 no_alignment == 2\n"
           "                                                               -}\n"
           "                                 , combinations    :: CInt    {- This is a flag set to true if we are going to accept\n"
           "                                                               - all possible combinations of the elements in the alphabet\n"
           "                                                               - in the alignments. This is not true for protein sequences\n"
           "                                                               - for example, where the number of elements of the alphabet\n"
           "                                                               - is already too big to build all the possible combinations.\n"
           "                                                               -}\n"
           "                                 , gapOpenCost   :: CInt      {- The cost of opening a gap. This is only useful in\n"
           "                                                               - certain cost_model_types (type 2: affine, based on my reading of ML code).\n"
           "                                                               -}\n"
           "                                 , isMetric      :: CInt      {- if tcm is symmetric\n"
           "                                                               - Not present in 3d. -}\n"
           "                                 , allElems      :: CInt      -- total number of elements TODO: figure out how this is different from alphSize\n"
           "                                 , bestCost      :: Ptr CInt  {- The transformation cost matrix, including ambiguities,\n"
           "                                                               - storing the **best** cost for each ambiguity pair\n"
           "                                                               -}\n"
           "                                 , medians       :: Ptr CUInt {- The matrix of possible medians between elements in the\n"
           "                                                               - alphabet. The best possible medians according to the cost\n"
           "                                                               - matrix.\n"
           "                                                               -}\n"
           "                                 , worstCost     :: Ptr CInt  {- The transformation cost matrix, including ambiguities,\n"
           "                                                               - storing the **worst** cost for each ambiguity pair\n"
           "                                                               - Missing in 3d\n"
           "                                                               -}\n"
           "                                 , prependCost   :: Ptr CInt  {- The cost of going from gap -> each base. For ambiguities, use best cost.\n"
           "                                                               - Set up as all_elements x all_elements\n"
           "                                                               - matrix, but seemingly only first row is used.\n"
           "                                                               - Missing in 3d because current version of 3d sets gap cost\n"
           "                                                               - as constant.\n"
           "                                                               -}\n"
           "                                 , tailCost      :: Ptr CInt  {- As prepend_cost, but with reverse directionality,\n"
           "                                                               - so base -> gap.\n"
           "                                                               - As with prepend_cost, seems to be allocated as too large.\n"
           "                                                               - Missing in 3d because current version of 3d sets gap cost\n"
           "                                                               - as constant.\n"
           "                                                               -}\n"
           "                                 }\n"
           "\n"
           "\n"
           "\n"
           "-- Because we\'re using a struct we need to make a Storable instance\n"
           "instance Storable CostMatrix3d where\n"
           "    sizeOf    _   = (", hsc_stdout());
#line 127 "externalCAlignment.hsc"
    hsc_size (struct seq);
    hsc_fputs (") -- #size is a built-in that works with arrays, as are #peek and #poke, below\n"
           "", hsc_stdout());
    hsc_line (128, "externalCAlignment.hsc");
    hsc_fputs ("    alignment _   = alignment (undefined :: CChar)\n"
           "    peek ptr      = do -- to get values from the C app\n"
           "        aSize     <- (", hsc_stdout());
#line 130 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, alphSize);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (131, "externalCAlignment.hsc");
    hsc_fputs ("        lcm\'      <- (", hsc_stdout());
#line 131 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, lcm);
    hsc_fputs (")             ptr\n"
           "", hsc_stdout());
    hsc_line (132, "externalCAlignment.hsc");
    hsc_fputs ("        gapchar   <- (", hsc_stdout());
#line 132 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_char);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (133, "externalCAlignment.hsc");
    hsc_fputs ("        costModel <- (", hsc_stdout());
#line 133 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost_model_type);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (134, "externalCAlignment.hsc");
    hsc_fputs ("        combos    <- (", hsc_stdout());
#line 134 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, combinations);
    hsc_fputs (")    ptr\n"
           "", hsc_stdout());
    hsc_line (135, "externalCAlignment.hsc");
    hsc_fputs ("        gapOpen   <- (", hsc_stdout());
#line 135 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_open);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (136, "externalCAlignment.hsc");
    hsc_fputs ("        metric    <- (", hsc_stdout());
#line 136 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, is_metric);
    hsc_fputs (")       ptr\n"
           "", hsc_stdout());
    hsc_line (137, "externalCAlignment.hsc");
    hsc_fputs ("        elems     <- (", hsc_stdout());
#line 137 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, all_elements);
    hsc_fputs (")    ptr\n"
           "", hsc_stdout());
    hsc_line (138, "externalCAlignment.hsc");
    hsc_fputs ("        best      <- (", hsc_stdout());
#line 138 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost);
    hsc_fputs (")            ptr\n"
           "", hsc_stdout());
    hsc_line (139, "externalCAlignment.hsc");
    hsc_fputs ("        meds      <- (", hsc_stdout());
#line 139 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, median);
    hsc_fputs (")          ptr\n"
           "", hsc_stdout());
    hsc_line (140, "externalCAlignment.hsc");
    hsc_fputs ("        worst     <- (", hsc_stdout());
#line 140 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, worst);
    hsc_fputs (")           ptr\n"
           "", hsc_stdout());
    hsc_line (141, "externalCAlignment.hsc");
    hsc_fputs ("        prepend   <- (", hsc_stdout());
#line 141 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, prepend_cost);
    hsc_fputs (")    ptr\n"
           "", hsc_stdout());
    hsc_line (142, "externalCAlignment.hsc");
    hsc_fputs ("        tail      <- (", hsc_stdout());
#line 142 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, tail_cost);
    hsc_fputs (")       ptr\n"
           "", hsc_stdout());
    hsc_line (143, "externalCAlignment.hsc");
    hsc_fputs ("        return  CostMatrix { alphSize      = aSize\n"
           "                           , lcm           = lcm\'\n"
           "                           , gapChar       = gapchar\n"
           "                           , costModelType = costmodel\n"
           "                           , combinations  = combos\n"
           "                           , gapOpenCost   = gapOpen\n"
           "                           , allElems      = elems\n"
           "                           , bestCost      = best\n"
           "                           , medians       = meds\n"
           "                           , worstCost     = worst\n"
           "                           , prependCost   = prepend\n"
           "                           , tailCost      = tail\n"
           "                       }\n"
           "\n"
           "    poke ptr (CostMatrix3d val seqFinal) = do -- to modify values in the C app\n"
           "        (", hsc_stdout());
#line 158 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, alphSize);
    hsc_fputs (")        ptr alphSize\n"
           "", hsc_stdout());
    hsc_line (159, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 159 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, lcm);
    hsc_fputs (")             ptr lcm\n"
           "", hsc_stdout());
    hsc_line (160, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 160 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_char);
    hsc_fputs (")        ptr gapChar\n"
           "", hsc_stdout());
    hsc_line (161, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 161 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost_model_type);
    hsc_fputs (") ptr costModelType\n"
           "", hsc_stdout());
    hsc_line (162, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 162 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, combinations);
    hsc_fputs (")    ptr combinations\n"
           "", hsc_stdout());
    hsc_line (163, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 163 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_open);
    hsc_fputs (")        ptr gapOpen\n"
           "", hsc_stdout());
    hsc_line (164, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 164 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, is_metric);
    hsc_fputs (")       ptr isMetric\n"
           "", hsc_stdout());
    hsc_line (165, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 165 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, all_elements);
    hsc_fputs (")    ptr elems\n"
           "", hsc_stdout());
    hsc_line (166, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 166 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost);
    hsc_fputs (")            ptr bestCost\n"
           "", hsc_stdout());
    hsc_line (167, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 167 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, median);
    hsc_fputs (")          ptr medians\n"
           "", hsc_stdout());
    hsc_line (168, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 168 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, worst);
    hsc_fputs (")           ptr worstCost\n"
           "", hsc_stdout());
    hsc_line (169, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 169 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, prepend_cost);
    hsc_fputs (")    ptr prependCost\n"
           "", hsc_stdout());
    hsc_line (170, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 170 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, tail_cost);
    hsc_fputs (")       ptr tailCost\n"
           "", hsc_stdout());
    hsc_line (171, "externalCAlignment.hsc");
    hsc_fputs ("\n"
           "\n"
           "{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}\n"
           "-- | Holds single cost matrix, which contains costs and medians for all\n"
           "-- possible sequence elements. It is completely filled using a TCM. See note below at \'setupCostMatrixFn_c\'.\n"
           "data CostMatrix3d = CostMatrix3d { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if\n"
           "                                                              --     combinations == True\n"
           "                                 , lcm           :: CInt      -- ceiling of log_2 (alphSize)\n"
           "                                 , gapChar       :: CInt      -- gap value (1 << (alphSize - 1))\n"
           "                                 , costModelType :: CInt      {- The type of cost model to be used in the alignment,\n"
           "                                                               - i.e. affine or not.\n"
           "                                                               - Based on cost_matrix.ml, values are:\n"
           "                                                               - \342\200\242 linear == 0\n"
           "                                                               - \342\200\242 affine == 1\n"
           "                                                               - \342\200\242 no_alignment == 2\n"
           "                                                               -}\n"
           "                                 , combinations    :: CInt     {- This is a flag set to true if we are going to accept\n"
           "                                                                - all possible combinations of the elements in the alphabet\n"
           "                                                                - in the alignments. This is not true for protein sequences\n"
           "                                                                - for example, where the number of elements of the alphabet\n"
           "                                                                - is already too big to build all the possible combinations.\n"
           "                                                                -}\n"
           "                                 , gapOpenCost   :: CInt        {- The cost of opening a gap. This is only useful in\n"
           "                                                                 - certain cost_model_types (type 2: affine, based on my reading of ML code).\n"
           "                                                                 -}\n"
           "                                 , isMetric      :: CInt        {- if tcm is symmetric\n"
           "                                                                 - Not present in 3d. -}\n"
           "                                 , allElems      :: CInt        -- total number of elements TODO: figure out how this is different from alphSize\n"
           "                                 , bestCost      :: Ptr CInt    {- The transformation cost matrix, including ambiguities,\n"
           "                                                                 - storing the **best** cost for each ambiguity pair\n"
           "                                                                 -}\n"
           "                                 , medians       :: Ptr CUInt   {- The matrix of possible medians between elements in the\n"
           "                                                                 - alphabet. The best possible medians according to the cost\n"
           "                                                                 - matrix.\n"
           "                                                                 -}\n"
           "                                 }\n"
           "\n"
           "\n"
           "\n"
           "-- Because we\'re using a struct we need to make a Storable instance\n"
           "instance Storable CostMatrix where\n"
           "    sizeOf    _   = (", hsc_stdout());
#line 212 "externalCAlignment.hsc"
    hsc_size (struct seq);
    hsc_fputs (") -- #size is a built-in that works with arrays, as are #peek and #poke, below\n"
           "", hsc_stdout());
    hsc_line (213, "externalCAlignment.hsc");
    hsc_fputs ("    alignment _   = alignment (undefined :: CChar)\n"
           "    peek ptr      = do -- to get values from the C app\n"
           "        aSize     <- (", hsc_stdout());
#line 215 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, alphSize);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (216, "externalCAlignment.hsc");
    hsc_fputs ("        lcm\'      <- (", hsc_stdout());
#line 216 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, lcm);
    hsc_fputs (")             ptr\n"
           "", hsc_stdout());
    hsc_line (217, "externalCAlignment.hsc");
    hsc_fputs ("        gapchar   <- (", hsc_stdout());
#line 217 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_char);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (218, "externalCAlignment.hsc");
    hsc_fputs ("        costModel <- (", hsc_stdout());
#line 218 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost_model_type);
    hsc_fputs (") ptr\n"
           "", hsc_stdout());
    hsc_line (219, "externalCAlignment.hsc");
    hsc_fputs ("        combos    <- (", hsc_stdout());
#line 219 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, combinations);
    hsc_fputs (")    ptr\n"
           "", hsc_stdout());
    hsc_line (220, "externalCAlignment.hsc");
    hsc_fputs ("        gapOpen   <- (", hsc_stdout());
#line 220 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_open);
    hsc_fputs (")        ptr\n"
           "", hsc_stdout());
    hsc_line (221, "externalCAlignment.hsc");
    hsc_fputs ("        metric    <- (", hsc_stdout());
#line 221 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, is_metric);
    hsc_fputs (")       ptr\n"
           "", hsc_stdout());
    hsc_line (222, "externalCAlignment.hsc");
    hsc_fputs ("        elems     <- (", hsc_stdout());
#line 222 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, all_elements);
    hsc_fputs (")    ptr\n"
           "", hsc_stdout());
    hsc_line (223, "externalCAlignment.hsc");
    hsc_fputs ("        best      <- (", hsc_stdout());
#line 223 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost);
    hsc_fputs (")            ptr\n"
           "", hsc_stdout());
    hsc_line (224, "externalCAlignment.hsc");
    hsc_fputs ("        meds      <- (", hsc_stdout());
#line 224 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, median);
    hsc_fputs (")          ptr\n"
           "", hsc_stdout());
    hsc_line (225, "externalCAlignment.hsc");
    hsc_fputs ("        return  CostMatrix { alphSize      = aSize\n"
           "                           , lcm           = lcm\'\n"
           "                           , gapChar       = gapchar\n"
           "                           , costModelType = costmodel\n"
           "                           , combinations  = combos\n"
           "                           , gapOpenCost   = gapOpen\n"
           "                           , allElems      = elems\n"
           "                           , bestCost      = best\n"
           "                           , medians       = meds\n"
           "                           }\n"
           "\n"
           "    poke ptr (CostMatrix val seqFinal) = do -- to modify values in the C app\n"
           "        (", hsc_stdout());
#line 237 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, alphSize);
    hsc_fputs (")        ptr alphSize\n"
           "", hsc_stdout());
    hsc_line (238, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 238 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, lcm);
    hsc_fputs (")             ptr lcm\n"
           "", hsc_stdout());
    hsc_line (239, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 239 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_char);
    hsc_fputs (")        ptr gapChar\n"
           "", hsc_stdout());
    hsc_line (240, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 240 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost_model_type);
    hsc_fputs (") ptr costModelType\n"
           "", hsc_stdout());
    hsc_line (241, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 241 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, combinations);
    hsc_fputs (")    ptr combinations\n"
           "", hsc_stdout());
    hsc_line (242, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 242 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, gap_open);
    hsc_fputs (")        ptr gapOpen\n"
           "", hsc_stdout());
    hsc_line (243, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 243 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, is_metric);
    hsc_fputs (")       ptr isMetric\n"
           "", hsc_stdout());
    hsc_line (244, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 244 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, all_elements);
    hsc_fputs (")    ptr elems\n"
           "", hsc_stdout());
    hsc_line (245, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 245 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, cost);
    hsc_fputs (")            ptr bestCost\n"
           "", hsc_stdout());
    hsc_line (246, "externalCAlignment.hsc");
    hsc_fputs ("        (", hsc_stdout());
#line 246 "externalCAlignment.hsc"
    hsc_peek (struct costMatrix, median);
    hsc_fputs (")          ptr medians\n"
           "", hsc_stdout());
    hsc_line (247, "externalCAlignment.hsc");
    hsc_fputs ("\n"
           "\n"
           "-- | Create and allocate cost matrix\n"
           "-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate\n"
           "-- the entire cost matrix, which includes ambiguous elements.\n"
           "-- TCM is row-major, with each row being the left sequence element.\n"
           "-- It is therefore indexed not by powers of two, but by cardinal integer.\n"
           "foreign import ccall unsafe \"c_code_alloc_setup.h setupCostMtx\"\n"
           "    setupCostMatrixFn_c :: Ptr CInt  -- tcm\n"
           "                        -> CInt      -- alphSize\n"
           "                        -> CInt      -- gap_open\n"
           "                        -> CInt      -- is_2d\n"
           "                        -> Ptr CostMatrix\n"
           "\n"
           "foreign import ccall unsafe \"c_code_alloc_setup.h initializeSeq\"\n"
           "    allocateSequenceFn_c :: CSize -> CSize -> Ptr Sequence\n"
           "\n"
           "\n"
           "\n"
           "\n"
           "-- | Get union of two sequences.\n"
           "-- Will only work if alignment and backtrace have already been called.\n"
           "-- First sequence must be shortest\n"
           "foreign import ccall unsafe \"algn.h algn_union\"\n"
           "    getUnionFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr Sequence\n"
           "\n"
           "-- | Will only work if alignment and backtrace have already been called.\n"
           "-- First sequence must be shortest\n"
           "foreign import ccall unsafe \"algn.h algn_get_median_2d_no_gaps\"\n"
           "    getUngappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence\n"
           "\n"
           "-- | Will only work if alignment and backtrace have already been called.\n"
           "-- First sequence must be shortest\n"
           "foreign import ccall unsafe \"algn.h algn_get_median_2d_with_gaps\"\n"
           "    getGappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence\n"
           "\n"
           "\n"
           "\n"
           "-- testFn can be called from within Haskell code.\n"
           "call2dSeqAlignFn_c :: Sequence -> Sequence -> CostMatrix -> NWMatrices -> Alignment\n"
           "call2dSeqAlignFn_c shortSeq longSeq costMatrix nwMatrices = unsafePerformIO $\n"
           "    -- have to allocate memory. Note that we\'re allocating via a lambda fn. I\n"
           "    -- don\'t yet understand what exactly is going on here.\n"
           "    alloca $ \\alignPtr -> do\n"
           "\n"
           "        -- Using strict here because the values need to be read before freeing,\n"
           "        -- so lazy is dangerous.\n"
           "        let !status = callExtFn_c arg1 arg2 3 3 alignPtr\n"
           "        free arg1\n"
           "        free arg2\n"
           "\n"
           "        -- Now checking return status. If 0, then all is well, otherwise throw an error.\n"
           "        if (fromIntegral status) == 0\n"
           "            then do\n"
           "                Sequence val seq <- peek alignPtr\n"
           "                seqStr           <- peekCAString seq\n"
           "                free seq\n"
           "                pure $ Right (fromIntegral val, seqStr)\n"
           "            else do\n"
           "                pure $ Left \"Out of memory\"\n"
           "\n"
           "-- Just for testing from CLI outside of ghci.\n"
           "main :: IO ()\n"
           "main = putStrLn $ show callSeqAlignFn_c\n"
           "\n"
           "\n"
           "", hsc_stdout());
    return 0;
}
