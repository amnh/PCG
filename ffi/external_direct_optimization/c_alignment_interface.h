#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "alignCharacters.h"
#include "alignmentMatrices.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "debug_constants.h"

/** Input/output structure for Haskell FFI. Essentially a cut down dyn_character_t.
 *
 *  Note that the character will be in the last `length` elements of the array.
 */
typedef struct alignIO_t {
    elem_t *character;
    size_t length;
    size_t capacity;
} alignIO_t;


/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last case the union will replace the gapped character placeholder.
 */
int align2d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *gappedOutput_aio
           , alignIO_t          *ungappedOutput_aio
           // , alignIO_t          *unionOutput_aio
           , cost_matrices_2d_t *costMtx2d
           , int                 getUngapped
           , int                 getGapped
           , int                 getUnion
           );


/** As align2d, but affine.
 *
 *  If `getMedians` gapped & ungapped outputs will be medians.
 */
int align2dAffine( alignIO_t          *inputChar1_aio
                 , alignIO_t          *inputChar2_aio
                 , alignIO_t          *gappedOutput_aio
                 , alignIO_t          *ungappedOutput_aio
                 // , alignIO_t          *unionOutput_aio
                 , cost_matrices_2d_t *costMtx2d_affine
                 , int                 getMedians
                 );


/** Aligns three characters using affine algorithm.
 *  Set `gap_open_cost` to equal `gap_extension_cost` for non-affine.
 *
 *  First declares, allocates and initializes data structures.
 *  Calls ukkCheckPoint.powell_3D_align().
 *  Calls alignCharacters.algn_get_cost_medians_3d().
 *  Copies output to correct return structures.
 *
 *  Ordering of inputs by length does not matter, as they will be sorted inside the fn.
 */
int align3d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *inputChar3_aio
           , alignIO_t          *outputChar1_aio
           , alignIO_t          *outputChar2_aio
           , alignIO_t          *outputChar3_aio
           , alignIO_t          *ungappedOutput_aio
           , alignIO_t          *gappedOutput_aio
           , cost_matrices_3d_t *costMtx3d
           , unsigned int        substitution_cost
           , unsigned int        gap_open_cost
           , unsigned int        gap_extension_cost
           );


/**  prints an alignIO struct */
void alignIO_print( const alignIO_t *character );


/** For use in 3D alignment.
 *
 *  Takes in three `alignIO` structs and a `characters_t`.
 *  _Copies_ respective characters from inputs to arrays in `output`. Must copy because characters start at end of `align_io` struct but
 *  at beginning of `characters_t` structs.
 *
 *  Nota bene: assumes that all arrays in `output` have already been allocated correctly.
 */
void alignIOtoCharacters_t( characters_t *output
                          , alignIO_t    *inputChar1_aio
                          , alignIO_t    *inputChar2_aio
                          , alignIO_t    *inputChar3_aio
                          );


/** For use in 2D alignment.
 *
 *  Takes in an `alignIO` struct and a `dyn_character` struct. Points interior pointers to input `alignIO`.
 *  Points `dyn_character->char_begin`, `dyn_character->end` to respective points in `alignIO->character`.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 *
 *  The values in the last `length` elements in `input` get copied to the _last_ `length` elements in the array in`retChar`.
 *
 *  This _does not_ allocate of copy values.
 *
 *  Nota bene: assumes that retChar->character has already been allocated correctly.
 */
void alignIOtoDynChar(       dyn_character_t *retChar
                     , const alignIO_t       *input
                     ,       size_t           alphabetSize
                     );


/** Allocate the fields of an alignIO struct. Incoming pointer must already have space for the alignIO field pointers allocated. */
void allocAlignIO( alignIO_t *toAlloc, size_t capacity );


/** For use in 3DO and for testing.
 *
 *  Copy an array of elem_t into an *already alloc'ed* (but see note below) alignIO struct. Then sets length to input length and
 *  capacity to input capacity.
 *
 *  Array values should fill *last* `length` elements of character buffer.
 *
 *  Allocates array that holds dynamic character of length capacity.
 */
void copyValsToAIO( alignIO_t *outChar, elem_t *vals, size_t length, size_t capacity );


/** For use in 3DO and for testing.
 *
 *  Copy section of character array that actually holds the character from alignIO to an *already alloc'ed* char array.
 *
 *  Does not allocate.
 */
void copyAioToVals( elem_t *vals, alignIO_t *inChar );


/** For use in 2D alignment code.
 *
 *  Takes in an alignIO and a dynamic character. *Copies* values of character from end of dynamic character to end of alignIO->character,
 *  so output must already be alloc'ed.
 *
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToAlignIO( alignIO_t *output, dyn_character_t *input, int delete_initial_gap );


void freeAlignIO( alignIO_t *toFree );


/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO( alignIO_t *inChar );


/** As allocAlignIO, but reallocs character. */
void reallocAlignIO( alignIO_t *toAlloc, size_t capacity );


#endif // C_ALIGNMENT_INTERFACE_H
