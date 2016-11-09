

//
//  main.c
//  haskellcode_version
//
//  Created by Yu Xiang on 10/17/15.
//  Copyright © 2015 Yu Xiang. All rights reserved.
//

// recent changes: 1. Changed all malloc()s to calloc()s. calloc() initializes memory, as well as allocating.
//                    This could very easily be changed back for speed efficiency, but it kept showing up as an error in valgrind.
//                 2. Added a return value of 2. Now 0 is success, 1 is memory allocation error, 2 is inputs were both 0 length.
//                 3. Changed strncpy(retAlign, _, _) to be a series of loops, each terminating at '*'.
//                    Also terminated both strings with '\0', for good measure.
//                 4. Made INIT_LENGTH a const.
//                 5. INIT_LENGTH = 2 * retAlign.alignmentLength - 1.
//                 6. Made LENGTH a const.
//                 7. Some style changes to enhance legibility for myself.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "seqAlignForHaskell.h"

int main() {
    
    char seqA_main[] = "CE\0";
    char seqB_main[] = "GCT\0";   //  Yu_Edit: here 100 is the maximum length of the sequences
    
    //    char seqA_main[100] = "HELLO", seqB_main[100] = "CC";   //  Yu_Edit: here 100 is the maximum length of the sequences
    
    //    char aligned[20];
    //struct align temp;
    
    //char seqB[10] = "ACGT";
    //char seqA[10] = "CACG";
    //
    //int wtInsertDel = 20;  //weight of insertion/deletion
    //int wtSub = 10;        //weigth of substitution
    
    
    int success = 1;
    struct retType retAlign;
    long int length = strlen(seqA_main) + strlen(seqB_main) + 5;
    retAlign.seq1 = calloc(length, sizeof(char));
    
    retAlign.seq2 = calloc(length, sizeof(char));
    if( retAlign.seq1 == NULL || retAlign.seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    // retAlign.seq1[length - 1] = '\0';
    // retAlign.seq2[length - 1] = '\0';
    retAlign.alignmentLength = length;
    
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
    
    
    success = aligner(seqA_main, seqB_main, wtInsertDel, wtSub, &retAlign);
    
    if (success == 0) {
        printf("\nSuccess!\n\n");
        printf("The aligned sequences are: \n%s\n%s\n", retAlign.seq1, retAlign.seq2);
        printf("The cost of the alignment is: %d\n", retAlign.weight);
        // for(int i = 0; i < length; ++i) {
        //     printf("%d\n",(int)retAlign.seq1[i]);
        // }
    } else {
        printf("Fail!\n");
    }
    free(retAlign.seq1);
    free(retAlign.seq2);
    
}





// EDIT: removed all // printf()s, as fn needs to be pure

// EDIT: return code 1 or 0 (failure or success), depending on whether malloc fails
// EDIT: also added pointer to struct align finalAlign, memory for which will be allocated in Haskell
// code, and which will hold the result

// EDIT: I changed the fn name to something more evocative.
// Obviously, feel free to make it yet more so.

int aligner(char *seq1, char *seq2, int wtInsertDel, int wtSub, struct retType* retAlign) {
    
    
    //Yu_Edit: changed the length of INIT_LENGTH
    const long int INIT_LENGTH = 2 * retAlign->alignmentLength - 1; // Will be used to initialize all subsequent alignment arrays.
    // Since a single array with hold both seqs, and each seq has length
    // alignmentLength, this number needs to be twice that, plus 1 for NULL.
    // However, the length was originally padded for a NULL, so one of those needs
    // to be removed.
    
    const long int LENGTH = retAlign->alignmentLength - 5; // strlen(seq1) + strlen(seq2);
    
    
    //printf("length is %ld\n", LENGTH);
    if (LENGTH == 0) {
        // printf("The lengths of the both sequences are zero!\n");
        return 2;
    } //else {
    //   INIT_LENGTH = 2 * LENGTH + 1; // 1 for NULL
    // }
    // printf( "Size of INIT_LENGTH: %ld\n", INIT_LENGTH);
    // printf( "size of alignment: %ld\n", retAlign->alignmentLength);
    
    //   // printf("INIT_LENGTH is %d\n", INIT_LENGTH);
    
    
    char* initArr = calloc( INIT_LENGTH, sizeof(char) );
    
    // Now, test for allocation. Return 1 if it fails.
    if( initArr == NULL ) {
        return 1;
    }
    for( int i = 0; i < INIT_LENGTH; i++ ) {
        initArr[i] = '*';
    }
    initArr[INIT_LENGTH-1] = '\0'; //do I need to do this? My C is a little rusty.
    
    // printf("initArr is %s\n", initArr);
    
    // printf("length of initArr is %ld\n", strlen(initArr));
    // END EDIT;
    
    // EDIT: In this and all future cases, I think we need to do similar malloc, strcopy, for dynamic allocation. I leave this to you, as I might be incorrect.
    //    struct align path[3] = { {0, 0, "********************", 0, 0, 0, 0, 1},
    //        {0, 0, "********************", 0, 0, 0, 0, 1},
    //        {0, 0, "********************", 0, 0, 0, 0, 1}};
    //
    
    //Yu_Edit  dynamically allocate struct
    //*******************  initialize struct ***************************
    struct align path[3] = {
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        },
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        },
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        }
    };
    
    for (int i = 0; i < 3; i++) {
        path[i].partialAlign = calloc( INIT_LENGTH, sizeof(char) );
        if( path[i].partialAlign == NULL ) {
            return 1;
        }
        strncpy(path[i].partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    }
    
    //    for (int i = 0; i < 3; i++) {
    //        strcpy(path[i].partialAlign, initArr);
    //    }
    
    
    // original metric
    // a(\sum z_i)^2 + b (\sum z_i) + c (\sum z_i^2)
    // WLOG let a = b=c = 1
    // (\sum z_i)^2 +  (\sum z_i) + (\sum z_i^2)
    
    
    // first metric
    // \sum z_i
    
    // second metric
    // \sum z_i^2
    
    
    
    
    //  struct align pathFirst[3];
    //  struct align pathSecond[3];
    
    
    
    
    
    struct align pathTempFirst[3] =
    {
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        },
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        },
        { .partialWt = 0,
            .partialTrueWt = 0,
            .posStringA = 0,
            .posStringB = 0,
            .posTrueA = 0,
            .posTrueB = 0,
            .flagWhichTree = 1
        }
    };
    
    for (int i = 0; i < 3; i++) {
        pathTempFirst[i].partialAlign = calloc( INIT_LENGTH, sizeof(char) );
        if( pathTempFirst[i].partialAlign == NULL ) {
            return 1;
        }
    }
    
    for (int i = 0; i < 3; i++) {
        strncpy(pathTempFirst[i].partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    }
    
    
    
    //    struct align *pathTempFirst = malloc((sizeof(struct align)) * 3);
    //
    //    if( pathTempFirst == NULL ) {
    //        return 1;
    //    }
    
    
    struct align pathTempSecond[3] = { {.partialWt = 0, .partialTrueWt = 0, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 1},
        {.partialWt = 0, .partialTrueWt = 0, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 1},
        {.partialWt = 0, .partialTrueWt = 0, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 1}};
    
    for (int i = 0; i < 3; i++) {
        pathTempSecond[i].partialAlign = calloc( INIT_LENGTH, sizeof(char) );
        if( pathTempSecond[i].partialAlign == NULL ) {
            return 1;
        }
    }
    
    for (int i = 0; i < 3; i++) {
        strncpy(pathTempSecond[i].partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    }
    
    
    struct align pathFirstInfinite = {.partialWt = 100000, .partialTrueWt = 100000, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 1};
    pathFirstInfinite.partialAlign = calloc( INIT_LENGTH, sizeof(char) );
    if( pathFirstInfinite.partialAlign == NULL ) {
        return 1;
    }
    strncpy(pathFirstInfinite.partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    
    struct align pathSecondInfinite = {.partialWt = 100000, .partialTrueWt = 100000, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 2};
    pathSecondInfinite.partialAlign = calloc( INIT_LENGTH, sizeof(char) );
    if( pathSecondInfinite.partialAlign == NULL ) {
        return 1;
    }
    strncpy(pathSecondInfinite.partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    
    //    struct align pathFirstInfinite = {100000, 100000, "********************", 0, 0, 0, 0, 1};
    //    struct align pathSecondInfinite = {100000, 100000, "********************", 0, 0, 0, 0, 2};
    
    
    //    struct align *pathTempSecond = malloc((sizeof(struct align)) * 3);
    //
    //    if( pathTempSecond == NULL ) {
    //        return 1;
    //    }
    
    //    struct align pathTempSecond[3];
    
    
    
    struct align finalAlign =
    {.partialWt = 0, .partialTrueWt = 0, .posStringA = 0, .posStringB = 0, .posTrueA = 0, .posTrueB = 0, .flagWhichTree = 1};
    
    
    finalAlign.partialAlign = calloc( INIT_LENGTH, sizeof(char) );
    if( finalAlign.partialAlign == NULL ) {
        return 1;
    }
    strncpy(finalAlign.partialAlign, initArr, sizeof(char) * INIT_LENGTH);
    
    
    
    //    struct align *finalAlign = malloc((sizeof(struct align)));
    //
    //    if( finalAlign == NULL ) {
    //        return 1;
    //    }
    
    //    struct align finalAlign;     // output the aligned sequences and the final distance
    
    
    char* seqA = calloc(strlen(seq1) + 1, sizeof(char));
    
    // Now, test for allocation. Return 1 if it fails.
    if( seqA == NULL ) {
        return 1;
    }
    char* seqB = calloc(strlen(seq2) + 1, sizeof(char));
    
    // Now, test for allocation. Return 1 if it fails.
    if( seqB == NULL ) {
        return 1;
    }
    
    strncpy(seqA, seq1, sizeof(char) * (strlen(seq1) + 1));
    strncpy(seqB, seq2, sizeof(char) * (strlen(seq2) + 1));
    
    // printf("seqA is %s\n", seqA);
    // printf("seqB is %s\n", seqB);
    
    //    char seqB[10];
    //    char seqA[10];
    //
    //    for(int i = 0;i < 10;i++){
    //        seqA[i] = seq1[i];
    //    }
    //    for(int i = 0;i < 10;i++){
    //        seqB[i] = seq2[i];
    //    }
    
    
    int flag = 0;
    int flagEmpty[2] = {1,1};   // indicator for the case when one tree becomes empty, i.e., all the candidates nodes have converged to the other tree
    
    int i;
    int j;
    int c, d, n = 9, swapA, swapB;  // for sorting
    int indicatorFirst, indicatorSecond, indicatorInitial, indicatorMix;
    int kInitial, kFirst, kSecond, kMix;
    //    int iMatch[3] = { 0, 0, 0};
    int iMatchFirst[3] = { 0, 0, 0};
    int iMatchSecond[3] = { 0, 0, 0};
    long int lengthSeqA, lengthSeqB;
    //  char alignFinal[20];
    
    
    char * alignFinal = calloc( INIT_LENGTH, sizeof(char) );
    if( alignFinal == NULL ) {
        return 1;
    }
    strncpy(alignFinal, initArr, sizeof(char) * INIT_LENGTH);
    
    
    //  int wtInsertDel = 20;  //weight of insertion/deletion
    //  int wtSub = 10;        //weigth of substitution
    //  int diff[20];           // difference between two sequences
    //  int wtTemp[6] = {0,0,0,0,0,0}, wtTempFirst[6] = {0,0,0,0,0,0}, wtTempSecond[6] = {0,0,0,0,0,0};
    //  int tempMatch;
    int iFirst = 0, iSecond = 0;
    //
    //    // test
    //    char arrayTest1[3] = "TTT";
    //    char arrayTest2[3];
    //  //   arrayTest1[2]='X';
    //
    //    for (i = 0; i < 3; i++) {
    //        arrayTest2[i]='C';
    //    }
    //
    //    arrayTest2[3]='\0';
    //    // printf("arrayTest1 is %s\n", arrayTest1);
    //   // // printf("last letter of arrayTest1 is %c\n", arrayTest1[2]);
    //    // printf("arrayTest2 is %s\n", arrayTest2);
    
    lengthSeqA = strlen(seqA);
    lengthSeqB = strlen(seqB);
    
    // printf("length of seqA: %ld\n", lengthSeqA);
    // printf("length of seqB: %ld\n", lengthSeqB);
    
    // // printf("%s\n", path[0].partialAlign );
    //
    //     first step in growing the tree
    //    struct align path[3] = {
    //        {wtSub, "A*********C*********", 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "A*********-*********", 1, 1, 1, 0},//inserstion
    //        {wtInsertDel, "-*********C*********", 1, 1, 0, 1}};//deletion
    //
    
    
    
    //*******************************  Initialization first level generation for both trees ************************
    
    struct align pathFirst[3] = {
        {.partialWt = wtSub, .partialTrueWt = wtSub + 2 * wtSub * wtSub, .posStringA = 1, .posStringB = 1, .posTrueA = 1, .posTrueB = 1, .flagWhichTree = 1},
        {.partialWt = wtInsertDel, .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel, .posStringA = 1, .posStringB = 1, .posTrueA = 1, .posTrueB = 0,.flagWhichTree = 1},
        {.partialWt = wtInsertDel, .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel, .posStringA = 1, .posStringB = 1, .posTrueA = 0, .posTrueB = 1, .flagWhichTree = 1}
        
    };
    //    struct align pathSecond[3];
    //    pathFirst[0] = {.partialWt = wtSub, .partialTrueWt = wtSub + 2 * wtSub * wtSub, .posStringA = 1, .posStringB = 1, .posTrueA = 1, .posTrueB = 1, .flagWhichTree = 1};
    //    pathFirst[1] = {
    //        .partialWt = wtInsertDel, .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel, .posStringA = 1, .posStringB = 1, .posTrueA = 1, .posTrueB = 0, .flagWhichTree = 1
    //    };
    //    pathFirst[2] = {
    //        .partialWt = wtInsertDel, .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel, .posStringA = 1, .posStringB = 1, .posTrueA = 0, .posTrueB = 1, .flagWhichTree = 1
    //    };
    
    for (i = 0; i < 3; i++) {
        pathFirst[i].partialAlign = calloc( INIT_LENGTH, sizeof(char) );
        if( pathFirst[i].partialAlign == NULL ) {
            return 1;
        }
        strncpy(pathFirst[i].partialAlign, initArr, sizeof(char) * INIT_LENGTH);
        
    }
    
    
    
    //    struct align pathFirst[3] = {
    //        {wtSub, wtSub + 2 * wtSub * wtSub, "********************", 1, 1, 1, 1, 1},//substitution
    //        {wtInsertDel, wtInsertDel + 2 * wtInsertDel * wtInsertDel,"********************", 1, 1, 1, 0, 1},//inserstion
    //        {wtInsertDel, wtInsertDel + 2 * wtInsertDel * wtInsertDel,"********************", 1, 1, 0, 1, 1}};//deletion
    //
    
    
    pathFirst[0].partialAlign[0] = seqA[0];
    pathFirst[0].partialAlign[LENGTH] = seqB[0];
    
    pathFirst[1].partialAlign[0] = seqA[0];
    pathFirst[1].partialAlign[LENGTH]='-';
    
    pathFirst[2].partialAlign[0]='-';
    pathFirst[2].partialAlign[LENGTH] = seqB[0];
    
    
    //    struct align pathSecond[3] = {
    //        {wtSub * wtSub, wtSub + 2 * wtSub * wtSub, "********************", 1, 1, 1, 1, 2},//substitution
    //        {wtInsertDel * wtInsertDel, wtInsertDel + 2 * wtInsertDel * wtInsertDel, "********************", 1, 1, 1, 0, 2},//inserstion
    //        {wtInsertDel * wtInsertDel, wtInsertDel + 2 * wtInsertDel * wtInsertDel, "********************", 1, 1, 0, 1, 2}};//deletion
    //
    
    struct align pathSecond[3] = {
        { .partialWt = wtSub * wtSub,
            .partialTrueWt = wtSub + 2 * wtSub * wtSub,
            .posStringA = 1,
            .posStringB = 1,
            .posTrueA = 1,
            .posTrueB = 1,
            .flagWhichTree = 2
        },
        { .partialWt = wtInsertDel * wtInsertDel,
            .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel,
            .posStringA = 1,
            .posStringB = 1,
            .posTrueA = 1,
            .posTrueB = 0,
            .flagWhichTree = 2
        },
        { .partialWt = wtInsertDel * wtInsertDel,
            .partialTrueWt = wtInsertDel + 2 * wtInsertDel * wtInsertDel,
            .posStringA = 1,
            .posStringB = 1,
            .posTrueA = 0,
            .posTrueB = 1,
            .flagWhichTree = 2
        }
    };
    
    
    
    for (i = 0; i < 3; i++) {
        pathSecond[i].partialAlign = calloc( INIT_LENGTH, sizeof(char) );
        if( pathSecond[i].partialAlign == NULL ) {
            return 1;
        }
        strncpy(pathSecond[i].partialAlign, initArr, sizeof(char) * INIT_LENGTH);
        
    }
    
    
    pathSecond[0].partialAlign[0] = seqA[0];
    pathSecond[0].partialAlign[LENGTH] = seqB[0];
    
    pathSecond[1].partialAlign[0] = seqA[0];
    pathSecond[1].partialAlign[LENGTH]='-';
    
    pathSecond[2].partialAlign[0]='-';
    pathSecond[2].partialAlign[LENGTH] = seqB[0];
    
    int arrayInitial[2][6]= {                         //  NEED UPDATE! now we assume seqA[0]! = seqB[0]
        { 10, 20, 30, 11, 21, 31 },
        { wtSub + 2 * wtSub * wtSub,
            wtInsertDel + 2 * wtInsertDel * wtInsertDel,
            wtInsertDel + 2 * wtInsertDel * wtInsertDel,
            wtSub + 2 * wtSub * wtSub,
            wtInsertDel + 2 * wtInsertDel * wtInsertDel,
            wtInsertDel + 2 * wtInsertDel * wtInsertDel
        }
    };
    
    for (c = 0 ; c < ( 6 - 1 ); c++)
    {
        for (d = 0 ; d < 6 - c - 1; d++)
        {
            if (arrayInitial[1][d] > arrayInitial[1][d + 1]) {
                swapA = arrayInitial[1][d];
                arrayInitial[1][d]   = arrayInitial[1][d + 1];
                arrayInitial[1][d + 1] = swapA;
                
                swapB = arrayInitial[0][d];
                arrayInitial[0][d]   = arrayInitial[0][d + 1];
                arrayInitial[0][d + 1] = swapB;
                
            }
        }
    }
    
    
    for (i = 0; i < 3; i++) {
        
        indicatorInitial = *( * (arrayInitial + 0) + i);   // decide which operation to make
        kInitial = *( * (arrayInitial + 0) + i) % 10;      // decide which path it belongs to
        
        if (kInitial == 0 && 9 < indicatorInitial && indicatorInitial < 12) {
            //   path[i] = pathFirst[0];
            path[i].partialWt = pathFirst[0].partialWt;
            path[i].partialTrueWt = pathFirst[0].partialTrueWt;
            path[i].posStringA = pathFirst[0].posStringA;
            path[i].posStringB = pathFirst[0].posStringB;
            path[i].posTrueA = pathFirst[0].posTrueA;
            path[i].posTrueB = pathFirst[0].posTrueB;
            path[i].flagWhichTree = pathFirst[0].flagWhichTree;
            strncpy(path[i].partialAlign, pathFirst[0].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        else if (kInitial == 0 && 19 < indicatorInitial && indicatorInitial < 22) {
            //     path[i] = pathFirst[1];
            
            path[i].partialWt = pathFirst[1].partialWt;
            path[i].partialTrueWt = pathFirst[1].partialTrueWt;
            path[i].posStringA = pathFirst[1].posStringA;
            path[i].posStringB = pathFirst[1].posStringB;
            path[i].posTrueA = pathFirst[1].posTrueA;
            path[i].posTrueB = pathFirst[1].posTrueB;
            path[i].flagWhichTree = pathFirst[1].flagWhichTree;
            strncpy(path[i].partialAlign, pathFirst[1].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        else if (kInitial == 0 && 29< indicatorInitial && indicatorInitial <32) {
            //      path[i] = pathFirst[2];
            
            path[i].partialWt = pathFirst[2].partialWt;
            path[i].partialTrueWt = pathFirst[2].partialTrueWt;
            path[i].posStringA = pathFirst[2].posStringA;
            path[i].posStringB = pathFirst[2].posStringB;
            path[i].posTrueA = pathFirst[2].posTrueA;
            path[i].posTrueB = pathFirst[2].posTrueB;
            path[i].flagWhichTree = pathFirst[2].flagWhichTree;
            strncpy(path[i].partialAlign, pathFirst[2].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        else if (kInitial == 1 && 9< indicatorInitial && indicatorInitial <12) {
            //  path[i] = pathSecond[0];
            
            path[i].partialWt = pathSecond[0].partialWt;
            path[i].partialTrueWt = pathSecond[0].partialTrueWt;
            path[i].posStringA = pathSecond[0].posStringA;
            path[i].posStringB = pathSecond[0].posStringB;
            path[i].posTrueA = pathSecond[0].posTrueA;
            path[i].posTrueB = pathSecond[0].posTrueB;
            path[i].flagWhichTree = pathSecond[0].flagWhichTree;
            strncpy(path[i].partialAlign, pathSecond[0].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        else if (kInitial == 1 && 19< indicatorInitial && indicatorInitial <22) {
            //      path[i] = pathSecond[1];
            
            path[i].partialWt = pathSecond[1].partialWt;
            path[i].partialTrueWt = pathSecond[1].partialTrueWt;
            path[i].posStringA = pathSecond[1].posStringA;
            path[i].posStringB = pathSecond[1].posStringB;
            path[i].posTrueA = pathSecond[1].posTrueA;
            path[i].posTrueB = pathSecond[1].posTrueB;
            path[i].flagWhichTree = pathSecond[1].flagWhichTree;
            strncpy(path[i].partialAlign, pathSecond[1].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        //   else path[i] = pathSecond[2];
        else{
            path[i].partialWt = pathSecond[2].partialWt;
            path[i].partialTrueWt = pathSecond[2].partialTrueWt;
            path[i].posStringA = pathSecond[2].posStringA;
            path[i].posStringB = pathSecond[2].posStringB;
            path[i].posTrueA = pathSecond[2].posTrueA;
            path[i].posTrueB = pathSecond[2].posTrueB;
            path[i].flagWhichTree = pathSecond[2].flagWhichTree;
            strncpy(path[i].partialAlign, pathSecond[2].partialAlign, sizeof(char) * INIT_LENGTH);
        }
        
    }
    
    
    
    //    for (i = 0; i < 3; i++) {                            // set all six nodes to be infinite nodes
    //        pathFirst[i] = pathFirstInfinite;
    //        pathSecond[i] = pathSecondInfinite;
    //    }
    
    for (i = 0; i < 3; i++) {
        pathFirst[i].partialWt = pathFirstInfinite.partialWt;
        pathFirst[i].partialTrueWt = pathFirstInfinite.partialTrueWt;
        pathFirst[i].posStringA = pathFirstInfinite.posStringA;
        pathFirst[i].posStringB = pathFirstInfinite.posStringB;
        pathFirst[i].posTrueA = pathFirstInfinite.posTrueA;
        pathFirst[i].posTrueB = pathFirstInfinite.posTrueB;
        pathFirst[i].flagWhichTree = pathFirstInfinite.flagWhichTree;
        strncpy(pathFirst[i].partialAlign, pathFirstInfinite.partialAlign, sizeof(char) * INIT_LENGTH);
        
        pathSecond[i].partialWt = pathSecondInfinite.partialWt;
        pathSecond[i].partialTrueWt = pathSecondInfinite.partialTrueWt;
        pathSecond[i].posStringA = pathSecondInfinite.posStringA;
        pathSecond[i].posStringB = pathSecondInfinite.posStringB;
        pathSecond[i].posTrueA = pathSecondInfinite.posTrueA;
        pathSecond[i].posTrueB = pathSecondInfinite.posTrueB;
        pathSecond[i].flagWhichTree = pathSecondInfinite.flagWhichTree;
        strncpy(pathSecond[i].partialAlign, pathSecondInfinite.partialAlign, sizeof(char) * INIT_LENGTH);
    }
    
    
    for (i = 0; i < 3; i++) {                            // assign three candidate nodes to the two trees and other nodes are infinite nodes
        if (path[i].flagWhichTree == 1) {
            //    pathFirst[iFirst] = path[i];
            
            pathFirst[iFirst].partialWt = path[i].partialWt;
            pathFirst[iFirst].partialTrueWt = path[i].partialTrueWt;
            pathFirst[iFirst].posStringA = path[i].posStringA;
            pathFirst[iFirst].posStringB = path[i].posStringB;
            pathFirst[iFirst].posTrueA = path[i].posTrueA;
            pathFirst[iFirst].posTrueB = path[i].posTrueB;
            pathFirst[iFirst].flagWhichTree = path[i].flagWhichTree;
            strncpy(pathFirst[iFirst].partialAlign, path[i].partialAlign, sizeof(char) * INIT_LENGTH);
            iFirst++;
        }
        else if(path[i].flagWhichTree == 2) {
            //     pathSecond[iSecond] = path[i];
            
            pathSecond[iSecond].partialWt = path[i].partialWt;
            pathSecond[iSecond].partialTrueWt = path[i].partialTrueWt;
            pathSecond[iSecond].posStringA = path[i].posStringA;
            pathSecond[iSecond].posStringB = path[i].posStringB;
            pathSecond[iSecond].posTrueA = path[i].posTrueA;
            pathSecond[iSecond].posTrueB = path[i].posTrueB;
            pathSecond[iSecond].flagWhichTree = path[i].flagWhichTree;
            strncpy(pathSecond[iSecond].partialAlign, path[i].partialAlign, sizeof(char) * INIT_LENGTH);
            iSecond++;
        }
    }
    
    
    // test initialization
    
    // printf("Three nodes in the first tree:\n");
    for (i = 0; i < 3; i++) {
        
        // printf("partialWt =%d, trueWt = %d, flag = %d", pathFirst[i].partialWt, pathFirst[i].partialTrueWt, pathFirst[i].flagWhichTree);
        // printf("partialAlign is: %s\n", pathFirst[i].partialAlign);
        
    }
    //        int a = 2;
    //        int b;
    //        b = pow(a,2);
    //        // printf("%d\n", pow(a,2));
    // printf("\n");
    // printf("Three nodes in the second tree:\n");
    for (i = 0; i < 3; i++) {
        
        // printf("partialWt =%d, trueWt = %d, flag = %d", pathSecond[i].partialWt, pathSecond[i].partialTrueWt, pathSecond[i].flagWhichTree);
        // printf("partialAlign is: %s\n", pathSecond[i].partialAlign);
    }
    
    //test function
    
    int temp;
    temp = trueWt(&pathFirst[0], wtInsertDel, wtSub, LENGTH);
    // printf("test weight is: %d\n", temp);
    //    path[i] = pathTempFirst[k];        // update the candidate paths
    //
    //    struct align pathTempInitial[3];
    //
    //    for (j = 0; j < 3; j++) {        // make a copy of previous paths
    //        pathTempInitial[j] = path[j];
    //    }
    //
    
    //    if (seqA[0] == seqB[0]) {
    //        tempMatch = 0;
    //    }
    //    else tempMatch = 1;
    //
    //    struct align path[3] = {
    //        {tempMatch * wtSub, "********************", 1, 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "********************", 1, 1, 1, 0, 1},//inserstion
    //        {wtInsertDel, "********************", 1, 1, 0, 1, 1}};//deletion
    
    //    // printf("path[0].partialAlign is %s\n", path[0].partialAlign);
    //
    //
    //    //struct align pathTemp[3];
    //    struct align test;
    //    test = path[2];
    //    // printf("test!!!!!%s\n", test.partialAlign);
    
    
    
    
    //    struct align path[3] = {
    //        {wtSub, "********************", 1, 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "********************", 1, 1, 1, 0, 1},//inserstion
    //        {wtInsertDel, "********************", 1, 1, 0, 1, 1}};//deletion
    //
    //
    //
    //    path[0].partialAlign[0] = seqA[0];
    //    path[0].partialAlign[10] = seqB[0];
    //
    //    path[1].partialAlign[0] = seqA[0];
    //    path[1].partialAlign[10]='-';
    //
    //    path[2].partialAlign[0]='-';
    //    path[2].partialAlign[10] = seqB[0];
    
    //********************************************************************************************************
    //********************************   grow both trees based on initialization ***************************
    //********************************************************************************************************
    
    do{
        
        // test output
        for (i = 0; i < 3; i++) {
            // printf("partial weight of path[%d] is %d\n", i, path[i].partialWt);
        }
        
        for (i = 0; i < 3; i++) {
            // printf("In path[%d], partial length of A is %d, partial length of B is %d\n", i, path[i].posStringA, path[i].posStringB);
            // printf("In path[%d], true length of A is %d, true length of B is %d\n", i, path[i].posTrueA, path[i].posTrueB);
        }
        
        // printf("\n");
        // printf("Three nodes in the first tree:\n");
        for (i = 0; i < 3; i++) {
            
            // printf("partialWt =%d, trueWt = %d, flag = %d", pathFirst[i].partialWt, pathFirst[i].partialTrueWt, pathFirst[i].flagWhichTree);
            // printf("partialAlign is: %s  ", pathFirst[i].partialAlign);
            // printf("position in seqA is:%d ", pathFirst[i].posTrueA);
            // printf("position in seqB is:%d\n", pathFirst[i].posTrueB);
            
            
        }
        // printf("\n");
        // printf("Three nodes in the second tree:\n");
        for (i = 0; i < 3; i++) {
            
            // printf("partialWt =%d, trueWt = %d, flag = %d", pathSecond[i].partialWt, pathSecond[i].partialTrueWt, pathSecond[i].flagWhichTree);
            // printf("partialAlign is: %s  ", pathSecond[i].partialAlign);
            // printf("position in seqA is:%d ", pathSecond[i].posTrueA);
            // printf("position in seqB is:%d\n", pathSecond[i].posTrueB);
        }
        // printf("\n");
        
        
        
        //*************************************** GROW TWO TREES BASED ON TWO METRICS  ******************************************************
        
        
        
        // grow tree according to first order metric: first tree
        
        for (i = 0; i < 3; i++) {
            
            // printf("%c, %c  ",seqA[pathFirst[i].posTrueA],seqB[pathFirst[i].posTrueB]);
            if (  seqA[pathFirst[i].posTrueA] == seqB[pathFirst[i].posTrueB]
                && pathFirst[i].posTrueA + 1 <= lengthSeqA
                && pathFirst[i].posTrueB + 1 <= lengthSeqB) {
                iMatchFirst[i] = 0;
                // printf("match or not? : %d\n", iMatchFirst[i]);
            } else if (seqA[pathFirst[i].posTrueA] != seqB[pathFirst[i].posTrueB]
                       && pathFirst[i].posTrueA + 1 <= lengthSeqA
                       && pathFirst[i].posTrueB + 1<= lengthSeqB) {
                iMatchFirst[i] = 1;
                // printf("match or not? : %d\n", iMatchFirst[i]);
            } else {
                iMatchFirst[i] = 1000;
                // printf("out of length: %d\n", iMatchFirst[i]);
            }
        }
        
        
        int arrayFirst[2][9]= {
            { 10, 20, 30, 11, 21, 31, 12, 22, 32 },
            { iMatchFirst[0] * wtSub + pathFirst[0].partialWt,
                wtInsertDel + pathFirst[0].partialWt,
                wtInsertDel + pathFirst[0].partialWt,
                iMatchFirst[1] * wtSub + pathFirst[1].partialWt,
                wtInsertDel + pathFirst[1].partialWt,
                wtInsertDel + pathFirst[1].partialWt,
                iMatchFirst[2] * wtSub + pathFirst[2].partialWt,
                wtInsertDel + pathFirst[2].partialWt,
                wtInsertDel + pathFirst[2].partialWt
            }
        };
        
        
        // grow tree according to second order metric: second tree
        
        
        for (i = 0; i < 3; i++) {
            
            // printf("%c, %c  ",seqA[pathSecond[i].posTrueA],seqB[pathSecond[i].posTrueB]);
            if(    seqA[pathSecond[i].posTrueA] == seqB[pathSecond[i].posTrueB]
               && pathSecond[i].posTrueA + 1<= lengthSeqA
               && pathSecond[i].posTrueB + 1<= lengthSeqB) {
                iMatchSecond[i] = 0;
                // printf("match or not? : %d\n", iMatchSecond[i]);
            } else if (seqA[pathSecond[i].posTrueA] != seqB[pathSecond[i].posTrueB]
                       && pathSecond[i].posTrueA + 1 <= lengthSeqA
                       && pathSecond[i].posTrueB + 1 <= lengthSeqB) {
                iMatchSecond[i] = 1;
                // printf("match or not? : %d\n", iMatchSecond[i]);
            } else {
                iMatchSecond[i] = 1000;
                // printf("out of length: %d\n", iMatchSecond[i]);
            }
        }
        
        
        int arraySecond[2][9]= {
            { 10, 20, 30, 11, 21, 31, 12, 22, 32 },
            { iMatchSecond[0] * wtSub * wtSub + pathSecond[0].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[0].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[0].partialWt,
                iMatchSecond[1] * wtSub * wtSub + pathSecond[1].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[1].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[1].partialWt,
                iMatchSecond[2] * wtSub * wtSub + pathSecond[2].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[2].partialWt,
                wtInsertDel * wtInsertDel + pathSecond[2].partialWt
            }
        };
        
        
        //************************************  SORT TWO TREES  *********************************************************
        
        // IMPORTANT TO BE UPDATED: WHEN THE THREE NODES CONVERGE TO ONE TREE, WE CAN FOCUS ON THAT TREE
        
        // first tree: sorting the second row by bubble and the first row change accordingly
        
        for (c = 0 ; c < ( n - 1 ); c++)
        {
            for (d = 0 ; d < n - c - 1; d++)
            {
                if (arrayFirst[1][d] > arrayFirst[1][d + 1]) {
                    swapA = arrayFirst[1][d];
                    arrayFirst[1][d]   = arrayFirst[1][d + 1];
                    arrayFirst[1][d + 1] = swapA;
                    
                    swapB = arrayFirst[0][d];
                    arrayFirst[0][d]   = arrayFirst[0][d + 1];
                    arrayFirst[0][d + 1] = swapB;
                    
                }
            }
        }
        
        // second tree: sorting the second row by bubble and the first row change accordingly
        
        for (c = 0; c < (n - 1); c++) {
            for (d = 0 ; d < n - c - 1; d++) {
                if (arraySecond[1][d] > arraySecond[1][d + 1]) {
                    swapA = arraySecond[1][d];
                    arraySecond[1][d]   = arraySecond[1][d + 1];
                    arraySecond[1][d + 1] = swapA;
                    
                    swapB = arraySecond[0][d];
                    arraySecond[0][d]   = arraySecond[0][d + 1];
                    arraySecond[0][d + 1] = swapB;
                    
                }
            }
        }
        
        
        
        //*************************************** grow first tree by obaining the three good nodes in first tree  ******************
        
        
        for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
            // in order to decide whether there is a match or substitution in the next position
            pathTempFirst[j].partialWt = pathFirst[j].partialWt;
            pathTempFirst[j].partialTrueWt = pathFirst[j].partialTrueWt;
            pathTempFirst[j].posStringA = pathFirst[j].posStringA;
            pathTempFirst[j].posStringB = pathFirst[j].posStringB;
            pathTempFirst[j].posTrueA = pathFirst[j].posTrueA;
            pathTempFirst[j].posTrueB = pathFirst[j].posTrueB;
            pathTempFirst[j].flagWhichTree = pathFirst[j].flagWhichTree;
            strncpy(pathTempFirst[j].partialAlign, pathFirst[j].partialAlign, sizeof(char) * INIT_LENGTH);
            //     pathTempFirst[j] = pathFirst[j];
            
        }
        
        //     {.partialWt = wtSub * wtSub, .partialTrueWt = wtSub + 2 * wtSub * wtSub, .posStringA = 1, .posStringB = 1, .posTrueA = 1, .posTrueB = 1, .flagWhichTree = 2}
        
        for (i = 0; i < 3; i++) {
            // printf("pathTempFirst[%d] is %s\n", i, pathTempFirst[i].partialAlign);
        }
        for (i = 0; i < 3; i++) {
            // printf("pathFirst[%d] is %s\n", i, pathFirst[i].partialAlign);
        }
        //
        //        pathFirst[0].partialAlign = "test";
        //        for (i = 0; i < 3; i++) {
        //            // printf("pathTempFirst[%d] is %s\n", i, pathTempFirst[i].partialAlign);
        //        }
        //        for (i = 0; i < 3; i++) {
        //            // printf("pathFirst[%d] is %s\n", i, pathFirst[i].partialAlign);
        //        }
        
        for (i = 0; i < 3; i++){
            
            indicatorFirst = *( *(arrayFirst + 0) + i);   // decide which operation to make
            kFirst = *( *(arrayFirst + 0) + i) % 10;      // decide which path it belongs to
            
            //  pathFirst[i] = pathTempFirst[kFirst];        // update the candidate paths
            
            pathFirst[i].partialWt = pathTempFirst[kFirst].partialWt;
            pathFirst[i].partialTrueWt = pathTempFirst[kFirst].partialTrueWt;
            pathFirst[i].posStringA = pathTempFirst[kFirst].posStringA;
            pathFirst[i].posStringB = pathTempFirst[kFirst].posStringB;
            pathFirst[i].posTrueA = pathTempFirst[kFirst].posTrueA;
            pathFirst[i].posTrueB = pathTempFirst[kFirst].posTrueB;
            pathFirst[i].flagWhichTree = pathTempFirst[kFirst].flagWhichTree;
            strncpy(pathFirst[i].partialAlign, pathTempFirst[kFirst].partialAlign, sizeof(char) * INIT_LENGTH);
            
            
            
            for (int l = 0; l < 3; l++) {
                // printf("pathTempFirst[%d] is %s\n", l, pathTempFirst[l].partialAlign);
            }
            // printf("test!!!!!!   pathTempFirst[%d].partialAlign is %s\n", kFirst, pathTempFirst[kFirst].partialAlign);
            // printf("path %d is chosen\n", kFirst);
            // printf("indicator is %d\n", indicatorFirst);
            
            //   // printf("test!!!!!!   pathTempFirst[%d].partialAlign is %s\n", i, pathTempFirst[i].partialAlign);
            // printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", i, pathFirst[i].partialAlign);
            
            if ( indicatorFirst > 9 && indicatorFirst < 15)  // substitution
            {
                
                pathFirst[i].partialWt = pathFirst[i].partialWt + iMatchFirst[kFirst] * wtSub;     // important!! iMatch[k] instead of iMatch[i]
                
                pathFirst[i].partialAlign[pathFirst[i].posStringA] = seqA[pathFirst[i].posTrueA];
                pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB] = seqB[pathFirst[i].posTrueB];
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                pathFirst[i].posTrueA++;
                pathFirst[i].posTrueB++;
                if (flagEmpty[0] == 0) {
                    
                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], wtInsertDel, wtSub, LENGTH);
                }
                
                for (int l = 0; l < 3; l++) {
                    // printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", l, pathFirst[l].partialAlign);
                }
                
                //                for (int l = 0; l < 3; l++) {
                //                    // printf("pathTempFirst[%d] is %s\n", l, pathTempFirst[l].partialAlign);
                //                }
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB - pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = '-';
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = '-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = seqA[pathFirst[i].posTrueA + j];
                        }
                    }
                    
                    //                    for (j = 0; j< = 2 * LENGTH; j++) {
                    //                        alignFinal[j] = pathFirst[i].partialAlign[j];
                    //
                    //                    }
                    strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                    // // printf("%s\n", alignFinal);
                    
                    // flagPath[i] = 1;
                    flag = 1;
                    break;
                }
                
            }
            
            if ( indicatorFirst > 19 && indicatorFirst < 25)    // gap in seqB
            {
                
                pathFirst[i].partialWt = pathFirst[i].partialWt + wtInsertDel;
                pathFirst[i].partialAlign[pathFirst[i].posStringA] = seqA[pathFirst[i].posTrueA];
                pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB]='-';
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                pathFirst[i].posTrueA++;
                if (flagEmpty[0] == 0) {
                    
                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], wtInsertDel, wtSub, LENGTH);
                }
                //  path[i].posTrueB++;
                
                for (int l = 0; l < 3; l++) {
                    // printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", l, pathFirst[l].partialAlign);
                }
                
                //// printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", i, pathFirst[i].partialAlign);
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = '-';
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = '-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = seqA[pathFirst[i].posTrueA + j];
                        }
                    }
                    
                    //                    for (j = 0; j< = 2 * LENGTH; j++) {
                    //                        alignFinal[j] = pathFirst[i].partialAlign[j];
                    //
                    //                    }
                    strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                    // // printf("%s\n", alignFinal);
                    
                    //  flagPath[i] = 1;
                    flag = 1;
                    break;
                }
            }
            
            if ( indicatorFirst > 29 && indicatorFirst < 35)    // gap in seqA
            {
                
                pathFirst[i].partialWt = pathFirst[i].partialWt + wtInsertDel;
                pathFirst[i].partialAlign[pathFirst[i].posStringA] = '-';
                pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB] = seqB[pathFirst[i].posTrueB];
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                // path[i].posTrueA++;
                pathFirst[i].posTrueB++;
                if (flagEmpty[0] == 0) {
                    
                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], wtInsertDel, wtSub, LENGTH);
                }
                
                
                for (int l = 0; l < 3; l++) {
                    // printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", l, pathFirst[l].partialAlign);
                }
                //  // printf("test!!!!!!   pathFirst[%d].partialAlign is %s\n", i, pathFirst[i].partialAlign);
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = '-';
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[LENGTH + pathFirst[i].posStringB + j] = '-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = seqA[pathFirst[i].posTrueA + j];
                        }
                    }
                    
                    //                    for (j = 0; j< = 2 * LENGTH; j++) {
                    //                        alignFinal[j] = pathFirst[i].partialAlign[j];
                    //
                    //                    }
                    strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                    // // printf("%s\n", alignFinal);
                    
                    //  flagPath[i] = 1;
                    flag = 1;
                    break;
                }
            }
            
            
        }
        
        
        for (i = 0; i < 3; i++) {
            // printf("pathFirst[%d] is %s\n",i,pathFirst[i].partialAlign);
        }
        
        for (i = 0; i < 3; i++) {
            // printf("pathFirst[%d].weight is %d\n",i,pathFirst[i].partialTrueWt);
        }
        
        //*************************************** grow second tree by obaining the three good nodes in second tree  ******************
        
        
        
        if (flag == 0) {
            
            
            
            for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
                // in order to decide whether there is a match or substitution in the next position
                //       pathTempSecond[j] = pathSecond[j];
                
                pathTempSecond[j].partialWt = pathSecond[j].partialWt;
                pathTempSecond[j].partialTrueWt = pathSecond[j].partialTrueWt;
                pathTempSecond[j].posStringA = pathSecond[j].posStringA;
                pathTempSecond[j].posStringB = pathSecond[j].posStringB;
                pathTempSecond[j].posTrueA = pathSecond[j].posTrueA;
                pathTempSecond[j].posTrueB = pathSecond[j].posTrueB;
                pathTempSecond[j].flagWhichTree = pathSecond[j].flagWhichTree;
                strncpy(pathTempSecond[j].partialAlign, pathSecond[j].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            
            // for (i = 0; i < 3; i++) {
            //     // printf("test one !!! pathSecond[%d].partialTrueWt is: %d\n", i, pathSecond[i].partialTrueWt);
            
            // }
            
            for (i = 0; i < 3; i++){
                
                indicatorSecond = *( * (arraySecond + 0) + i);   // decide which operation to make
                kSecond = *( * (arraySecond + 0) + i) % 10;      // decide which path it belongs to
                
                //     pathSecond[i] = pathTempSecond[kSecond];        // update the candidate paths
                
                
                pathSecond[i].partialWt = pathTempSecond[kSecond].partialWt;
                pathSecond[i].partialTrueWt = pathTempSecond[kSecond].partialTrueWt;
                pathSecond[i].posStringA = pathTempSecond[kSecond].posStringA;
                pathSecond[i].posStringB = pathTempSecond[kSecond].posStringB;
                pathSecond[i].posTrueA = pathTempSecond[kSecond].posTrueA;
                pathSecond[i].posTrueB = pathTempSecond[kSecond].posTrueB;
                pathSecond[i].flagWhichTree = pathTempSecond[kSecond].flagWhichTree;
                strncpy(pathSecond[i].partialAlign, pathTempSecond[kSecond].partialAlign, sizeof(char) * INIT_LENGTH);
                
                
                // printf("path %d is chosen\n", kSecond);
                // printf("indicator is %d\n", indicatorSecond);
                
                
                if ( indicatorSecond > 9 && indicatorSecond < 15)  // substitution
                {
                    
                    pathSecond[i].partialWt = pathSecond[i].partialWt + iMatchSecond[kSecond] * wtSub * wtSub; //Important! iMatch[k] instead ofiMatch[i]
                    
                    pathSecond[i].partialAlign[pathSecond[i].posStringA] = seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB] = seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    pathSecond[i].posTrueA++;
                    pathSecond[i].posTrueB++;
                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], wtInsertDel, wtSub, LENGTH);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]='-';
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j] = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        
                        //                        for (j = 0; j< = 2 * LENGTH; j++) {
                        //                            alignFinal[j] = pathSecond[i].partialAlign[j];
                        //
                        //                        }
                        
                        strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                        // // printf("%s\n", alignFinal);
                        
                        // flagPath[i] = 1;
                        flag = 1;
                        break;
                    }
                    
                }
                
                if ( indicatorSecond > 19 && indicatorSecond < 25)    // gap in seqB
                {
                    
                    pathSecond[i].partialWt = pathSecond[i].partialWt + wtInsertDel * wtInsertDel;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA] = seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB]='-';
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    pathSecond[i].posTrueA++;
                    //  path[i].posTrueB++;
                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], wtInsertDel, wtSub, LENGTH);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]='-';
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j] = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        
                        //                        for (j = 0; j< = 2 * LENGTH; j++) {
                        //                            alignFinal[j] = pathSecond[i].partialAlign[j];
                        //
                        //                        }
                        strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                        // // printf("%s\n", alignFinal);
                        
                        //  flagPath[i] = 1;
                        flag = 1;
                        break;
                    }
                }
                
                if ( indicatorSecond > 29 && indicatorSecond < 35)    // gap in seqA
                {
                    
                    pathSecond[i].partialWt = pathSecond[i].partialWt + wtInsertDel * wtInsertDel;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]='-';
                    pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB] = seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    // path[i].posTrueA++;
                    pathSecond[i].posTrueB++;
                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], wtInsertDel, wtSub, LENGTH);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]='-';
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[LENGTH + pathSecond[i].posStringB + j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j] = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        
                        //                        for (j = 0; j< = 2 * LENGTH; j++) {
                        //                            alignFinal[j] = pathSecond[i].partialAlign[j];
                        //
                        //                        }
                        strncpy(alignFinal, pathFirst[i].partialAlign, sizeof(char) * INIT_LENGTH);
                        // // printf("%s\n", alignFinal);
                        
                        //  flagPath[i] = 1;
                        flag = 1;
                        break;
                    }
                }
            }
            for (i = 0; i < 3; i++) {
                // // printf("test two !!! pathSecond[%d].partialTrueWt is: %d\n", i, pathSecond[i].partialTrueWt);
                
            }
        }
        
        //****************************************   sort the six nodes according to the true metric *************************************************
        
        int arrayMix[2][9]= {
            {10, 20, 30, 11, 21, 31},
            {pathFirst[0].partialTrueWt, pathFirst[1].partialTrueWt, pathFirst[2].partialTrueWt,
                pathSecond[0].partialTrueWt, pathSecond[1].partialTrueWt, pathSecond[2].partialTrueWt}
        };
        
        for (c = 0 ; c < ( 6 - 1 ); c++)
        {
            for (d = 0 ; d < 6 - c - 1; d++)
            {
                if (arrayMix[1][d] > arrayMix[1][d + 1]) {
                    swapA = arrayMix[1][d];
                    arrayMix[1][d]   = arrayMix[1][d + 1];
                    arrayMix[1][d + 1] = swapA;
                    
                    swapB = arrayMix[0][d];
                    arrayMix[0][d]   = arrayMix[0][d + 1];
                    arrayMix[0][d + 1] = swapB;
                    
                }
            }
        }
        
        
        for (i = 0; i < 3; i++) {
            
            indicatorMix = *( * (arrayMix + 0) + i);   // decide which operation to make
            kMix = *( * (arrayMix + 0) + i) % 10;      // decide which path it belongs to
            
            if (kMix == 0 && 9< indicatorMix && indicatorMix <12) {
                //      path[i] = pathFirst[0];
                
                path[i].partialWt = pathFirst[0].partialWt;
                path[i].partialTrueWt = pathFirst[0].partialTrueWt;
                path[i].posStringA = pathFirst[0].posStringA;
                path[i].posStringB = pathFirst[0].posStringB;
                path[i].posTrueA = pathFirst[0].posTrueA;
                path[i].posTrueB = pathFirst[0].posTrueB;
                path[i].flagWhichTree = pathFirst[0].flagWhichTree;
                strncpy(path[i].partialAlign, pathFirst[0].partialAlign, sizeof(char) * INIT_LENGTH);
                
            }
            else if (kMix == 0 && 19< indicatorMix && indicatorMix <22) {
                //    path[i] = pathFirst[1];
                
                path[i].partialWt = pathFirst[1].partialWt;
                path[i].partialTrueWt = pathFirst[1].partialTrueWt;
                path[i].posStringA = pathFirst[1].posStringA;
                path[i].posStringB = pathFirst[1].posStringB;
                path[i].posTrueA = pathFirst[1].posTrueA;
                path[i].posTrueB = pathFirst[1].posTrueB;
                path[i].flagWhichTree = pathFirst[1].flagWhichTree;
                strncpy(path[i].partialAlign, pathFirst[1].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            else if (kMix == 0 && 29< indicatorMix && indicatorMix <32) {
                //     path[i] = pathFirst[2];
                
                path[i].partialWt = pathFirst[2].partialWt;
                path[i].partialTrueWt = pathFirst[2].partialTrueWt;
                path[i].posStringA = pathFirst[2].posStringA;
                path[i].posStringB = pathFirst[2].posStringB;
                path[i].posTrueA = pathFirst[2].posTrueA;
                path[i].posTrueB = pathFirst[2].posTrueB;
                path[i].flagWhichTree = pathFirst[2].flagWhichTree;
                strncpy(path[i].partialAlign, pathFirst[2].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            else if (kMix == 1 && 9< indicatorMix && indicatorMix <12) {
                //   path[i] = pathSecond[0];
                
                path[i].partialWt = pathSecond[0].partialWt;
                path[i].partialTrueWt = pathSecond[0].partialTrueWt;
                path[i].posStringA = pathSecond[0].posStringA;
                path[i].posStringB = pathSecond[0].posStringB;
                path[i].posTrueA = pathSecond[0].posTrueA;
                path[i].posTrueB = pathSecond[0].posTrueB;
                path[i].flagWhichTree = pathSecond[0].flagWhichTree;
                strncpy(path[i].partialAlign, pathSecond[0].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            else if (kMix == 1 && 19< indicatorMix && indicatorMix <22) {
                //      path[i] = pathSecond[1];
                
                path[i].partialWt = pathSecond[1].partialWt;
                path[i].partialTrueWt = pathSecond[1].partialTrueWt;
                path[i].posStringA = pathSecond[1].posStringA;
                path[i].posStringB = pathSecond[1].posStringB;
                path[i].posTrueA = pathSecond[1].posTrueA;
                path[i].posTrueB = pathSecond[1].posTrueB;
                path[i].flagWhichTree = pathSecond[1].flagWhichTree;
                strncpy(path[i].partialAlign, pathSecond[1].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            //  else path[i] = pathSecond[2];
            else
            {
                path[i].partialWt = pathSecond[2].partialWt;
                path[i].partialTrueWt = pathSecond[2].partialTrueWt;
                path[i].posStringA = pathSecond[2].posStringA;
                path[i].posStringB = pathSecond[2].posStringB;
                path[i].posTrueA = pathSecond[2].posTrueA;
                path[i].posTrueB = pathSecond[2].posTrueB;
                path[i].flagWhichTree = pathSecond[2].flagWhichTree;
                strncpy(path[i].partialAlign, pathSecond[2].partialAlign, sizeof(char) * INIT_LENGTH);
            }
            
        }
        
        
        
        //****************************************   assign nodes for the next round *************************************************
        
        
        iFirst = 0;
        iSecond = 0;
        
        
        for (i = 0; i < 3; i++) {                            // set all six nodes to be infinite nodes
            //   pathFirst[i] = pathFirstInfinite;
            
            pathFirst[i].partialWt = pathFirstInfinite.partialWt;
            pathFirst[i].partialTrueWt = pathFirstInfinite.partialTrueWt;
            pathFirst[i].posStringA = pathFirstInfinite.posStringA;
            pathFirst[i].posStringB = pathFirstInfinite.posStringB;
            pathFirst[i].posTrueA = pathFirstInfinite.posTrueA;
            pathFirst[i].posTrueB = pathFirstInfinite.posTrueB;
            pathFirst[i].flagWhichTree = pathFirstInfinite.flagWhichTree;
            strncpy(pathFirst[i].partialAlign, pathFirstInfinite.partialAlign, sizeof(char) * INIT_LENGTH);
            
            //    pathSecond[i] = pathSecondInfinite;
            pathSecond[i].partialWt = pathSecondInfinite.partialWt;
            pathSecond[i].partialTrueWt = pathSecondInfinite.partialTrueWt;
            pathSecond[i].posStringA = pathSecondInfinite.posStringA;
            pathSecond[i].posStringB = pathSecondInfinite.posStringB;
            pathSecond[i].posTrueA = pathSecondInfinite.posTrueA;
            pathSecond[i].posTrueB = pathSecondInfinite.posTrueB;
            pathSecond[i].flagWhichTree = pathSecondInfinite.flagWhichTree;
            strncpy(pathSecond[i].partialAlign, pathSecondInfinite.partialAlign, sizeof(char) * INIT_LENGTH);
        }
        
        for (i = 0; i < 3; i++) {                            // assign three candidate nodes to the two trees and other nodes are infinite nodes
            if (path[i].flagWhichTree == 1) {
                //    pathFirst[iFirst] = path[i];
                pathFirst[iFirst].partialWt = path[i].partialWt;
                pathFirst[iFirst].partialTrueWt = path[i].partialTrueWt;
                pathFirst[iFirst].posStringA = path[i].posStringA;
                pathFirst[iFirst].posStringB = path[i].posStringB;
                pathFirst[iFirst].posTrueA = path[i].posTrueA;
                pathFirst[iFirst].posTrueB = path[i].posTrueB;
                pathFirst[iFirst].flagWhichTree = path[i].flagWhichTree;
                strncpy(pathFirst[iFirst].partialAlign, path[i].partialAlign, sizeof(char) * INIT_LENGTH);
                iFirst++;
            }
            else if(path[i].flagWhichTree == 2) {
                // pathSecond[iSecond] = path[i];
                pathSecond[iSecond].partialWt = path[i].partialWt;
                pathSecond[iSecond].partialTrueWt = path[i].partialTrueWt;
                pathSecond[iSecond].posStringA = path[i].posStringA;
                pathSecond[iSecond].posStringB = path[i].posStringB;
                pathSecond[iSecond].posTrueA = path[i].posTrueA;
                pathSecond[iSecond].posTrueB = path[i].posTrueB;
                pathSecond[iSecond].flagWhichTree = path[i].flagWhichTree;
                strncpy(pathSecond[iSecond].partialAlign, path[i].partialAlign, sizeof(char) * INIT_LENGTH);
                iSecond++;
            }
        }
        
        
        
        for (i = 0; i < 3; i++) {
            if(path[i].flagWhichTree == 1){
                flagEmpty[0] = 0;
                break;
            }
        }
        for (i = 0; i < 3; i++) {
            if (path[i].flagWhichTree == 2) {
                flagEmpty[1] = 0;
                break;
            }
        }
        
        
        
        
        
        
        
        // // printf("test three flagEmpty[0] = %d, flagEmpty[1] = %d\n", flagEmpty[0], flagEmpty[1]);
        
        //****************************************   print out tree for testing *************************************************
        
        //        // print first tree for testing
        //        // printf("First tree:");
        //        for (i = 0; i < 2; i++) {
        //            for (j = 0; j < 9; j++) {
        //                // printf("%d ", arrayFirst[i][j]);
        //            }
        //            // printf("\n");
        //        }
        
        
        // print second tree for testing
        //        // printf("Second tree:");
        //        for (i = 0; i < 2; i++) {
        //            for (j = 0; j < 9; j++) {
        //                // printf("%d ", arraySecond[i][j]);
        //            }
        //            // printf("\n");
        //        }
        
        
        
    } while(flag == 0);
    
    
    strncpy(finalAlign.partialAlign, alignFinal, sizeof(char) * INIT_LENGTH);
    //    for(i = 0; i < 2 * LENGTH; i++){
    //        finalAlign[0].partialAlign[i] = alignFinal[i];
    //    }
    
    finalAlign.partialWt = 0;
    
    for(i = 0; i < LENGTH; i++){
        if(finalAlign.partialAlign[i] == '-' || finalAlign.partialAlign[i + LENGTH] == '-')
            finalAlign.partialWt = finalAlign.partialWt + wtInsertDel;
        else if(finalAlign.partialAlign[i] == finalAlign.partialAlign[i + LENGTH])
            finalAlign.partialWt = finalAlign.partialWt;
        else
            finalAlign.partialWt = finalAlign.partialWt + wtSub;
        
    }
    
    
    //  // printf("the final weight is:%d\n", finalAlign.partialWt);
    
    
    // EDIT: here I'm assigning to retAlign. You might have a better way to do this.
    int strIdx = 0;
    while( finalAlign.partialAlign[strIdx] != '*' && finalAlign.partialAlign[strIdx] != '\0' ) {
        retAlign->seq1[strIdx] = finalAlign.partialAlign[strIdx];
        strIdx++;
    }
    retAlign->seq1[strIdx] = '\0';
    
    while( finalAlign.partialAlign[strIdx] == '*' && finalAlign.partialAlign[strIdx] != '\0' ) { strIdx++; }
    int normalizer = strIdx;
    while( finalAlign.partialAlign[strIdx] != '*' && finalAlign.partialAlign[strIdx] != '\0' ) {
        retAlign->seq2[strIdx - normalizer] = finalAlign.partialAlign[strIdx];
        strIdx++;
    }
    retAlign->seq2[strIdx - normalizer] = '\0';
    
    // strncpy(retAlign->seq1, finalAlign.partialAlign, retAlign->alignmentLength);
    // strncpy(retAlign->seq2, finalAlign.partialAlign, retAlign->alignmentLength);
    retAlign->weight = finalAlign.partialWt;
    // printf("%s %s\n\n", retAlign->seq1, retAlign->seq2);
    
    free(initArr);
    
    
    
    
    for (int i = 0; i < 3; i++) {
        free(path[i].partialAlign);
        
    }
    free(pathFirstInfinite.partialAlign);
    free(pathSecondInfinite.partialAlign);
    //
    
    //    free(pathFirstInfinite);
    //    free(pathSecondInfinite);
    
    for (i = 0; i < 3; i++) {
        free(pathFirst[i].partialAlign);
        free(pathSecond[i].partialAlign);
        
    }
    for (i = 0; i < 3; i++) {
        free(pathTempFirst[i].partialAlign);
        free(pathTempSecond[i].partialAlign);
        
    }
    
    //    free(pathTempFirst);
    //    free(pathTempSecond);
    
    free(seqA);
    free(seqB);
    free(finalAlign.partialAlign);
    free(alignFinal);
    // EDIT: returning success code.
    return 0;
}

//****************************************   COMBINE SORT CANDIDATES ACCORDING TO TRUE METRIC  *************************************************

int trueWt(struct align *path, int wtInsertDel, int wtSub, int len){
    
    int i;
    
    int *diff = calloc(len, sizeof(int));           // difference between two sequences
    
    for (i = 0; i < len; i++) {
        diff[i] = 0;
    }
    //   a =malloc(sizeof(int) * 10);
    
    
    //    int diff[10] = {0,0,0,0,0,0,0,0,0,0};           // difference between two sequences
    // int wtTemp = 0,
    // int wtSub = 10;
    // int wtInsertDel = 20;
    int wtTempFirst = 0, wtTempSecond = 0;
    int wtTemp;
    
    
    // // printf("function value is: %s\n", path->partialAlign);
    // // printf("path->posStringA is :%d\n", path->posStringA);
    
    //   for(j = 0;j < 3;j++){
    
    for(i = 0; i < path->posStringA ; i++){
        
        if (path->partialAlign[i] == '-' || path->partialAlign[i + len] == '-') {
            diff[i] = wtInsertDel;
        }
        else if (path->partialAlign[i]== path->partialAlign[i + len]) {
            diff[i] = 0;
        }
        else {
            diff[i] = wtSub;
        }
    }
    //    }
    
    for(i = 0; i < path->posStringA; i++){
        
        wtTempFirst = wtTempFirst + diff[i];
    }
    wtTempFirst = wtTempFirst * wtTempFirst + wtTempFirst;
    
    // // printf("wtTempFirst is: %d\n", wtTempFirst);
    for(i = 0; i < path->posStringA ; i++){
        
        wtTempSecond = wtTempSecond + diff[i] * diff[i];
    }
    
    // // printf("wtTempSecond is: %d\n", wtTempSecond);
    wtTemp = wtTempFirst + wtTempSecond;
    
    // test output
    
    // // printf("wtTemp is:%d\n", wtTemp);
    
    free(diff);
    return wtTemp;
    
}

