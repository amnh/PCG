//
//  main.c
//  version_Haskell_bit
//
//  Created by Yu Xiang on 11/1/16.
//  Copyright © 2016 Yu Xiang. All rights reserved.
//


// recent changes: 1. Changed all malloc()s to calloc()s. calloc() initializes memory, as well as allocating.
//                    This could very easily be changed back for speed efficiency, but it kept showing up as an error in valgrind.
//                 2. Added a return value of 2. Now 0 is success, 1 is memory allocation error, 2 is inputs were both 0 length.
//                 3. Changed strncpy(retAlign, _, _) to be a series of loops, each terminating at '*'.
//                    Also terminated both strings with '\0', for good measure.
//                 4. Made INIT_LENGTH a const.
//                 5. INIT_LENGTH = 2 * retAlign->alignmentLength - 1.
//                 6. Made LENGTH a const.
//                 7. Some style changes to enhance legibility for myself.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "dynamicCharacterOperations.h"
#include "seqAlignForHaskell.h"



int main() {
    
   // char seqA_main[] = "CE\0";
   // char seqB_main[] = "GCT\0";   //  Yu_Edit: here 100 is the maximum length of the sequences
    
    // Since we're only dealing with dna + gap now:
    size_t alphSize = 5;
    int seqA_main[] = {1, 2, 3};
    size_t seqALen = 3;
    int seqB_main[] = {4, 5, 6};   //  Yu_Edit: here 100 is the maximum length of the sequences
    size_t seqBLen = 3;

    
    //int wtInsertDel = 20;  //weight of insertion/deletion
    //int wtSub = 10;        //weigth of substitution
    
    
    int success = 1;
    retType_t* retAlign = malloc( sizeof(retType_t) );
//    long int length = strlen(seqA_main) + strlen(seqB_main) + 5;
    long int length = sizeof(seqA_main)/sizeof(seqA_main[0]) + sizeof(seqB_main)/sizeof(seqB_main[0]) + 5;
    
 //   retAlign->seq1 = calloc(length, sizeof(char));
    retAlign->seq1 = calloc(length, sizeof(int));
  //  retAlign->seq2 = calloc(length, sizeof(char));
    retAlign->seq2 = calloc(length, sizeof(int));
    
    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    // retAlign->seq1[length - 1] = '\0';
    // retAlign->seq2[length - 1] = '\0';
    retAlign->alignmentLength = length;
    
    //    // printf("Please input two sequences A and B each of length at most 10 over alphabet {A,T,C,G}.\n");
    //    // printf("Input sequence A:\n");
    //    scanf(" %s", seqA);                         // important to have a space before %s!
    //    // printf("sequence A = %s\n", seqA);
    //
    //    // printf("Input sequence B:\n");
    //    scanf(" %s", seqB);
    //    // printf("sequence B = %s\n", seqB);
    
    int wtSub = 1, wtInsertDel = 1;
    
    //    // printf("Please input weights of substitution and insertion/deletion (both of them are integers less than 50)\n");
    //    // printf("Input weight of substitution:\n");
    //    scanf("%d", &wtSub);
    //    // printf("weight of substitution = %d\n", wtSub);
    //
    //    // printf("Input weight of insertion/deletion:\n");
    //    scanf("%d", &wtInsertDel);
    //    // printf("weight of insertion/deletion = %d\n", wtInsertDel);
    
    
    success = aligner(seqA_main, seqALen, seqB_main, seqBLen, alphSize, wtInsertDel, wtSub, retAlign);
    
    if (success == 0) {
        printf("\nSuccess!\n\n");
        printf("The aligned sequences are: \n%p\n%p\n", retAlign->seq1, retAlign->seq2);
        printf("The cost of the alignment is: %d\n", retAlign->weight);
        // for(int i = 0; i < length; ++i) {
        //     printf("%d\n",(int)retAlign->seq1[i]);
        // }
    } else {
        printf("Fail!\n");
    }
    free(retAlign->seq1);
    free(retAlign->seq2);
    
}


/** 
 *  A sample program that takes in two dynamic characters and returns two aligned dynamic chars,
 *  in the form of an alignResult_t. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
int exampleInterfaceFn(dynChar_t* seqA, dynChar_t* seqB, alignResult_t* result) {

    int* seqA_main = dynCharToIntArr(seqA);
    int* seqB_main = dynCharToIntArr(seqB);

    retType_t* retAlign = malloc( sizeof(retType_t) );
    
    long int length = seqA->numElems + seqB->numElems + 5;
    retAlign->seq1 = calloc(length, sizeof(int));
    retAlign->seq2 = calloc(length, sizeof(int));
    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    // Call aligner with TCM here: int success = aligner(seqA_main, seqB_main, costMtx_t* tcm, &retAlign);
    // The following as an example
    int success = aligner(seqA_main, seqA->numElems, seqB_main, seqB->numElems, 2, 1, seqA->alphSize, retAlign);
    result->finalChar1 = intArrToBitArr (seqA->alphSize, retAlign->alignmentLength, retAlign->seq1);
    result->finalChar2 = intArrToBitArr (seqA->alphSize, retAlign->alignmentLength, retAlign->seq2);

    result->finalWt      = retAlign->weight;
    result->finalLength  = retAlign->alignmentLength;

    freeRetType(retAlign);
    free(retAlign);

    return 0;
}