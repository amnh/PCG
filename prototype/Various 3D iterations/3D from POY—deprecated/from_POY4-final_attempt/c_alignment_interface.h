#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "alignCharacters.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"
#include "alignCharacters.h"

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
 *  In the last two cases the union will replace the gapped character placeholder.
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


/** As align2d, but affine */
int align2dAffine( alignIO_t          *inputChar1_aio
                 , alignIO_t          *inputChar2_aio
                 , alignIO_t          *gappedOutput_aio
                 , alignIO_t          *ungappedOutput_aio
                 // , alignIO_t          *unionOutput_aio
                 , cost_matrices_2d_t *costMtx2d_affine
                 , int                 getMedians
                 );


/** Aligns three characters using affine algorithm.
 *  Set `gap_open_cost` to 0 for non-affine.
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
           , alignIO_t          *ungappedOutput_aio
           , alignIO_t          *gappedOutput_aio
           , cost_matrices_3d_t *costMtx3d
           , unsigned int        gap_open_cost
           // , unsigned int        gap_extension_cost
           );


/**  prints an alignIO struct */
void alignIO_print( const alignIO_t *character );


/** Takes in an alignIO struct and a dyn_character struct. Copies values of alignIO to dyn_character.
 *  Points dyn_character->char_begin, dyn_character->end to respective points in alignIO->character.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 *
 *  The values in the last `length` elements in `input` get copied to the last `length` elements in the array in`retChar`.
 *
 *  Nota bene: assumes that retChar->character has already been allocated correctly.
 */
void alignIOtoDynChar(       dyn_character_t *retChar
                     , const alignIO_t       *input
                     ,       size_t           alphabetSize
                     );


/** Allocate the fields of an alignIO struct. Incoming pointer must already have space for the alignIO field pointers allocated. */
void allocAlignIO(alignIO_t *toAlloc, size_t capacity);


/** Copy an array of elem_t into an *already alloc'ed* alignIO struct. Then sets length to input length and
 *  capacity to input capacity.
 *
 *  Array values should fill last `length` elements of character buffer.
 *
 *  Does not allocate.
 */
void copyValsToAIO(alignIO_t *outChar, elem_t *vals, size_t length, size_t capacity);


/** Takes in an alignIO and a char. *Copies* values of character from end of char to end of alignIO->character, so output must already
 *  be alloc'ed.
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToAlignIO(alignIO_t *output, dyn_character_t *input);


void freeAlignIO(alignIO_t *toFree);


/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_t *inChar);


/** As allocAlignIO, but reallocs character. */
void reallocAlignIO(alignIO_t *toAlloc, size_t capacity);



#endif // C_ALIGNMENT_INTERFACE_H
