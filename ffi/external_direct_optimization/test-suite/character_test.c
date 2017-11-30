#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../alignCharacters.h"
#include "../c_alignment_interface.h"
#include "../c_code_alloc_setup.h"
#include "../debug_constants.h"
#include "../costMatrix.h"
#include "../alignmentMatrices.h"
#include "../ukkCheckPoint.h"
#include "../ukkCommon.h"

#define ALPHABET_SIZE 5
#define BASE_A        1 << 0
#define BASE_C        1 << 1
#define BASE_G        1 << 2
#define BASE_T        1 << 3
#define BASE_GAP      1 << 4


void print_usage(char * progName) {
    printf("\n");
    printf("Expected usage: %s <gap_open_cost> <char_1> <char_2> <char_3>\n", progName);
    printf("  <gap_open_cost>  Non-negative Integer, required\n");
    printf("  <char_1>         IUPAC DNA String,     required\n");
    printf("  <char_2>         IUPAC DNA String,     required\n");
    printf("  <char_3>         IUPAC DNA String,     optional\n");
    printf("\n");
    printf("  If <gap_open_cost> is 0, non-affine alignment will be performed.\n");
    printf("  If <gap_open_cost> is a positive value, an affine alignment will\n");
    printf("                     be performed with using <gap_open_cost>.\n");
    printf("\n");
    printf("  If <char_3> is missing, a pairwise  alignment will be performed.\n");
    printf("  If <char_3> is present, a three-way alignment will be performed.\n");
    printf("\n");
}


alignIO_t* read_dynamic_character(size_t buffer_capacity, char * str) {

    size_t i;
    size_t valid_chars     = 0;
    size_t input_length    = strlen(str);
    elem_t *element_buffer = malloc( sizeof(*element_buffer) * buffer_capacity );
    assert( element_buffer != NULL && "Couldn't allocate element buffer when reading dyn character." );

    // Infer ambiguous elements from IUPAC codes
    for (i = valid_chars = 0; i < input_length; ++i) {
        switch (str[i]) {
            case '-': element_buffer[valid_chars++] = BASE_GAP; break;

            case 'A': element_buffer[valid_chars++] = BASE_A; break;
            case 'C': element_buffer[valid_chars++] = BASE_C; break;
            case 'G': element_buffer[valid_chars++] = BASE_G; break;
            case 'T':
            case 'U': element_buffer[valid_chars++] = BASE_T; break;

            case 'R': element_buffer[valid_chars++] = BASE_A | BASE_G; break;
            case 'Y': element_buffer[valid_chars++] = BASE_C | BASE_T; break;
            case 'S': element_buffer[valid_chars++] = BASE_G | BASE_C; break;
            case 'W': element_buffer[valid_chars++] = BASE_A | BASE_T; break;
            case 'K': element_buffer[valid_chars++] = BASE_G | BASE_T; break;
            case 'M': element_buffer[valid_chars++] = BASE_A | BASE_C; break;

            case 'B': element_buffer[valid_chars++] = BASE_C | BASE_G | BASE_T; break;
            case 'D': element_buffer[valid_chars++] = BASE_A | BASE_G | BASE_T; break;
            case 'H': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_T; break;
            case 'V': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_G; break;

            case 'N': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_G | BASE_T; break;

            // Lower case means include gap
            case 'a': element_buffer[valid_chars++] = BASE_A | BASE_GAP; break;
            case 'c': element_buffer[valid_chars++] = BASE_C | BASE_GAP; break;
            case 'g': element_buffer[valid_chars++] = BASE_G | BASE_GAP; break;
            case 't':
            case 'u': element_buffer[valid_chars++] = BASE_T | BASE_GAP; break;

            case 'r': element_buffer[valid_chars++] = BASE_A | BASE_G | BASE_GAP; break;
            case 'y': element_buffer[valid_chars++] = BASE_C | BASE_T | BASE_GAP; break;
            case 's': element_buffer[valid_chars++] = BASE_G | BASE_C | BASE_GAP; break;
            case 'w': element_buffer[valid_chars++] = BASE_A | BASE_T | BASE_GAP; break;
            case 'k': element_buffer[valid_chars++] = BASE_G | BASE_T | BASE_GAP; break;
            case 'm': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_GAP; break;

            case 'b': element_buffer[valid_chars++] = BASE_C | BASE_G | BASE_T | BASE_GAP; break;
            case 'd': element_buffer[valid_chars++] = BASE_A | BASE_G | BASE_T | BASE_GAP; break;
            case 'h': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_T | BASE_GAP; break;
            case 'v': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_G | BASE_GAP; break;

            case 'n':
            case '?': element_buffer[valid_chars++] = BASE_A | BASE_C | BASE_G | BASE_T | BASE_GAP; break;
    }
        /*
        printf("str[%zu]: '%c' ", i, str[i]);
        printf("element_buffer: [ ");
    for(size_t j = 0; j < valid_chars; ++j) {
            printf("%d ", element_buffer[j]);
    }
    printf("]\n");
        */
    }

    // Shift elements to the end of the buffer
    // We move backwards through the buffer to prevent clobbering input
    int src, des;
    for (src = valid_chars - 1, des = buffer_capacity - 1; 0 <= src; --src, --des) {
        element_buffer[des] = element_buffer[src];
    }

    // Initialize structure
    alignIO_t *input_character = malloc( sizeof(*input_character) );
    assert( input_character != NULL && "Couldn't allocate alignIO_t struct in reading dyn character." );

    input_character->character = element_buffer;
    input_character->length    = valid_chars;
    input_character->capacity  = buffer_capacity;
    return input_character;
}


int main(int argc, char* argv[]) {

    char* progName = argv[0];

    if (argc < 3) {
        printf("ERROR:\n  Less than 3 parameters supplied!\n");
        print_usage(progName);
    }

    size_t gap_open_cost;
    int read_success;

    // Get the "gap open cost" parameter
    read_success = sscanf(argv[1], "%zu", &gap_open_cost);
    if(!read_success) {
        printf("ERROR:\n  First parameter was not a non-negative, integral value.\n");
        print_usage(progName);
    }

    int is_3d_alignment = argc > 4;

    // Get the sum of the length of the dynamic characters
    size_t length1 = strlen(argv[2]);
    size_t length2 = strlen(argv[3]);
    size_t length3 = (!is_3d_alignment) ? 0 : strlen(argv[4]);
    size_t buffer_capacity = length1 + length2 + length3;

    // Get the "dynamic character" parameters
    alignIO_t *align_value_1 = read_dynamic_character (buffer_capacity, argv[2]);
    alignIO_t *align_value_2 = read_dynamic_character (buffer_capacity, argv[3]);
    alignIO_t *align_value_3 = (!is_3d_alignment) ? NULL : read_dynamic_character (buffer_capacity, argv[4]);

    // Allocate space for the output values.
    alignIO_t *align_value_gapped   = malloc( sizeof(*align_value_gapped  ) );
    alignIO_t *align_value_ungapped = malloc( sizeof(*align_value_ungapped) );
    assert(   align_value_gapped   != NULL
           && align_value_ungapped != NULL
           && "Couldn't allocate median alignIO_t struct in reading dyn character." );

    allocAlignIO(align_value_gapped  , buffer_capacity);
    allocAlignIO(align_value_ungapped, buffer_capacity);


    /************  Allocate cost matrices  **************/

    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left character element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    size_t tcm_total_len  = ALPHABET_SIZE * ALPHABET_SIZE; // the size of the input tcm
    unsigned int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    assert( tcm != NULL && "Couldn't allocate siple tcm when testing character structs." );

    for (size_t i = 0; i < tcm_total_len; i += ALPHABET_SIZE) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < ALPHABET_SIZE; j++) {
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * ALPHABET_SIZE ) {
                tcm[i + j] = IDENTITY_COST; // identity
            } else if (i == (tcm_total_len - ALPHABET_SIZE) || j == (ALPHABET_SIZE - 1)) {
                tcm[i + j] = INDEL_COST;    // indel cost
            } else {
                tcm[i + j] = SUB_COST;      // sub cost
            }
         }
    }

    /************  Perform Alignment  **************/

    int algnCost;

    // Print inputs
    printf("Captured Inputs:\n\n");
    alignIO_print(align_value_1);
    alignIO_print(align_value_2);
    if (is_3d_alignment)
        alignIO_print(align_value_3);

    // Perform 2D alignment
    if( !is_3d_alignment ) {
        cost_matrices_2d_t *costMtx2d = malloc(sizeof(struct cost_matrices_2d_t));
        assert( costMtx2d != NULL && "Couldn't reallocate costMtx2d when testing character structs." );

        setUp2dCostMtx (costMtx2d, tcm, ALPHABET_SIZE, gap_open_cost);

        // Non-affine alignment
    if( gap_open_cost == 0 )
            algnCost = align2d( align_value_1
                              , align_value_2
                              , align_value_gapped
                              , align_value_ungapped
                              , costMtx2d
                              , 0                    // do ungapped
                              , 0                    // do gapped
                              , 0                    // do union
                      );
        // Affine alignment
    else
            algnCost = align2dAffine( align_value_1
                                    , align_value_2
                                    , align_value_gapped
                                    , align_value_ungapped
                                    , costMtx2d
                                    , 0                 // compute medians
                                    );
    }

    // Perform 3D, maybe affine alignment
    else {
        cost_matrices_3d_t *costMtx3d = malloc(sizeof(struct cost_matrices_3d_t));
        assert( costMtx3d != NULL && "Couldn't reallocate costMtx3d when testing character structs." );

        setUp3dCostMtx (costMtx3d, tcm, ALPHABET_SIZE, 0);
        algnCost = align3d( align_value_1
                          , align_value_2
                          , align_value_3
                          , align_value_gapped
                          , align_value_ungapped
                          , costMtx3d
                          , gap_open_cost
                          );
    }

    // Print calculated outputs
    printf("\nCalculated output:\n\n");
    printf("Alignment cost: %d\n", algnCost);

    printf("\nAligned input characters\n");
    alignIO_print(align_value_1);
    alignIO_print(align_value_2);
    if (is_3d_alignment)
        alignIO_print(align_value_3);

    printf("\n  Gapped character  ");
    alignIO_print(align_value_gapped);

    printf("\n  Ungapped character  ");
    alignIO_print(align_value_ungapped);
}
