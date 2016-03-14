//
//  main.c
//  haskellcode_version
//
//  Created by Yu Xiang on 10/17/15.
//  Copyright © 2015 Yu Xiang. All rights reserved.
//

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "yuAlign.h"

// EDIT: removed main()
// EDIT: removed all printf()s, as fn needs to be pure

// EDIT: return code 1 or 0 (failure or success), depending on whether malloc fails
// EDIT: also added pointer to struct align finalAlign, memory for which will be allocated in Haskell
// code, and which will hold the result

// EDIT: I changed the fn name to something more evocative. 
// Obviously, feel free to make it yet more so.
int aligner(char *seq1, char *seq2, int wtInsertDel, int wtSub, struct retType* retAlign){
    
//    printf("test!!!!!!!!!%s\n", seq1);
    
    // EDIT: added this to make array size dynamic (I needed to for testing purposes.). Maybe better with a memset? Dunno.
    int initLength = strlen(seq1) + strlen(seq1) + 1; // 1 for NULL
    char* initArr = malloc(( sizeof(char) ) * initLength);

    // Now, test for allocation. Return 1 if it fails.
    if( initArr == NULL ) {
        return 1;
    }
    for( int i = 0; i < initLength; i++ ) {
        initArr[i] = '*';
    }
    initArr[initLength-1] = '\0'; //do I need to do this? My C is a little rusty.
    // END EDIT;

    // EDIT: In this and all future cases, I think we need to do similar malloc, strcopy, for dynamic allocation. I leave this to you, as I might be incorrect.
    struct align path[3]={ {0, 0, "********************", 0, 0, 0, 0, 1},
        {0, 0, "********************", 0, 0, 0, 0, 1},
        {0, 0, "********************", 0, 0, 0, 0, 1}};
    
    // original metric
    // a(\sum z_i)^2 + b (\sum z_i) + c (\sum z_i^2)
    // WLOG let a=b=c=1
    // (\sum z_i)^2 +  (\sum z_i) + (\sum z_i^2)
    
    
    // first metric
    // \sum z_i
    
    // second metric
    // \sum z_i^2
    
    struct align pathFirstInfinite = {100000, 100000, "********************", 0, 0, 0, 0, 1};
    struct align pathSecondInfinite = {100000, 100000, "********************", 0, 0, 0, 0, 2};
    
    
  //  struct align pathFirst[3];
  //  struct align pathSecond[3];
    
    struct align pathTempFirst[3];
    struct align pathTempSecond[3];
    
    struct align finalAlign;     // output the aligned sequences and the final distance
    //char seqB[10] = "ACGT";
    //char seqA[10] = "CACG";
    //
    //int wtInsertDel=20;  //weight of insertion/deletion
    //int wtSub=10;        //weigth of substitution
    
    char seqB[10];
    char seqA[10];
    
    for(int i=0;i<10;i++){
        seqA[i]=seq1[i];
    }
    for(int i=0;i<10;i++){
        seqB[i]=seq2[i];
    }

//    printf("test!!!! %s\n", seqA);
//    char seqB[10] = "AAAA";
//    char seqA[10] = "CCCC";
    
//    int wtInsertDel=1;  //weight of insertion/deletion
//    int wtSub=10;        //weigth of substitution
    
    //int wtTemp=0;
    int flag=0;
    int flagFirstTree = 0, flagSecondTree = 0;
    int numPathsFirstTree=0, numPathsSecondTree = 0;
    int flagPath[3]={0,0,0};
    int flagEmpty[2]={1,1};   // indicator for the case when one tree becomes empty, i.e., all the candidates nodes have converged to the other tree
    
    //int *sortRow(int wtInsertDel, int wtSub);
    
    
    int i;
    int j;
    int c, d, n=9, swapA, swapB;  // for sorting
    int indicatorFirst, indicatorSecond, indicatorInitial, indicatorMix;
    int kInitial, kFirst, kSecond, kMix;
    //    int iMatch[3]={ 0, 0, 0};
    int iMatchFirst[3]={ 0, 0, 0};
    int iMatchSecond[3]={ 0, 0, 0};
    long int lengthSeqA, lengthSeqB;
    char alignFinal[20];
    //  int wtInsertDel=20;  //weight of insertion/deletion
    //  int wtSub=10;        //weigth of substitution
    //  int diff[20];           // difference between two sequences
    //  int wtTemp[6] = {0,0,0,0,0,0}, wtTempFirst[6]={0,0,0,0,0,0}, wtTempSecond[6]={0,0,0,0,0,0};
    //  int tempMatch;
    int iFirst=0, iSecond=0;
    //
    //    // test
    //    char arrayTest1[3]="TTT";
    //    char arrayTest2[3];
    //  //   arrayTest1[2]='X';
    //
    //    for (i=0; i<3; i++) {
    //        arrayTest2[i]='C';
    //    }
    //
    //    arrayTest2[3]='\0';
    //    printf("arrayTest1 is %s\n", arrayTest1);
    //   // printf("last letter of arrayTest1 is %c\n", arrayTest1[2]);
    //    printf("arrayTest2 is %s\n", arrayTest2);
    
    lengthSeqA = strlen(seqA);
    lengthSeqB = strlen(seqB);
    
    // printf("length of seqA: %ld\n", lengthSeqA);
    // printf("length of seqB: %ld\n", lengthSeqB);
    
    // printf("%s\n", path[0].partialAlign );
    //
    //     first step in growing the tree
    //    struct align path[3]={
    //        {wtSub, "A*********C*********", 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "A*********-*********", 1, 1, 1, 0},//inserstion
    //        {wtInsertDel, "-*********C*********", 1, 1, 0, 1}};//deletion
    //
    
    
    
    //*******************************  Initialization first level generation for both trees ************************
    
    struct align pathFirst[3]={
        {wtSub, wtSub+2*wtSub*wtSub, "********************", 1, 1, 1, 1, 1},//substitution
        {wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel,"********************", 1, 1, 1, 0, 1},//inserstion
        {wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel,"********************", 1, 1, 0, 1, 1}};//deletion
    
    
    
    pathFirst[0].partialAlign[0]=seqA[0];
    pathFirst[0].partialAlign[10]=seqB[0];
    
    pathFirst[1].partialAlign[0]=seqA[0];
    pathFirst[1].partialAlign[10]='-';
    
    pathFirst[2].partialAlign[0]='-';
    pathFirst[2].partialAlign[10]=seqB[0];
    
    
    struct align pathSecond[3]={
        {wtSub*wtSub, wtSub+2*wtSub*wtSub, "********************", 1, 1, 1, 1, 2},//substitution
        {wtInsertDel*wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel, "********************", 1, 1, 1, 0, 2},//inserstion
        {wtInsertDel*wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel, "********************", 1, 1, 0, 1, 2}};//deletion
    
    
    
    pathSecond[0].partialAlign[0]=seqA[0];
    pathSecond[0].partialAlign[10]=seqB[0];
    
    pathSecond[1].partialAlign[0]=seqA[0];
    pathSecond[1].partialAlign[10]='-';
    
    pathSecond[2].partialAlign[0]='-';
    pathSecond[2].partialAlign[10]=seqB[0];
    
    int arrayInitial[2][6]= {                         //  NEED UPDATE! now we assume seqA[0]!=seqB[0]
        {10, 20, 30, 11, 21, 31},
        {wtSub+2*wtSub*wtSub, wtInsertDel+2*wtInsertDel*wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel,
            wtSub+2*wtSub*wtSub, wtInsertDel+2*wtInsertDel*wtInsertDel, wtInsertDel+2*wtInsertDel*wtInsertDel}
    };
    
    for (c = 0 ; c < ( 6 - 1 ); c++)
    {
        for (d = 0 ; d < 6 - c - 1; d++)
        {
            if (arrayInitial[1][d] > arrayInitial[1][d+1]) {
                swapA = arrayInitial[1][d];
                arrayInitial[1][d]   = arrayInitial[1][d+1];
                arrayInitial[1][d+1] = swapA;
                
                swapB = arrayInitial[0][d];
                arrayInitial[0][d]   = arrayInitial[0][d+1];
                arrayInitial[0][d+1] = swapB;
                
            }
        }
    }
    
    
    for (i=0; i<3; i++) {
        
        indicatorInitial = *(*(arrayInitial + 0) + i);   // decide which operation to make
        kInitial = *(*(arrayInitial + 0) + i) % 10;      // decide which path it belongs to
        
        if (kInitial==0 && 9< indicatorInitial && indicatorInitial <12) {
            path[i]=pathFirst[0];
        }
        else if (kInitial==0 && 19< indicatorInitial && indicatorInitial <22) {
            path[i]=pathFirst[1];
        }
        else if (kInitial==0 && 29< indicatorInitial && indicatorInitial <32) {
            path[i]=pathFirst[2];
        }
        else if (kInitial==1 && 9< indicatorInitial && indicatorInitial <12) {
            path[i]=pathSecond[0];
        }
        else if (kInitial==1 && 19< indicatorInitial && indicatorInitial <22) {
            path[i]=pathSecond[1];
        }
        else path[i]=pathSecond[2];
        
    }
    
    
    
    for (i=0; i<3; i++) {                            // set all six nodes to be infinite nodes
        pathFirst[i]=pathFirstInfinite;
        pathSecond[i]=pathSecondInfinite;
    }
    
    for (i=0; i<3; i++) {                            // assign three candidate nodes to the two trees and other nodes are infinite nodes
        if (path[i].flagWhichTree==1) {
            pathFirst[iFirst]=path[i];
            iFirst++;
        }
        else if(path[i].flagWhichTree==2) {
            pathSecond[iSecond]=path[i];
            iSecond++;
        }
    }
    
    
    // test initialization
    
    // printf("Three nodes in the first tree:\n");
    // for (i=0; i<3; i++) {
        
    //     printf("partialWt =%d, trueWt=%d, flag=%d", pathFirst[i].partialWt, pathFirst[i].partialTrueWt, pathFirst[i].flagWhichTree);
    //     printf("partialAlign is: %s\n", pathFirst[i].partialAlign);
        
    // }
    //    int a=2;
    //    int b;
    //    b=pow(a,2);
    //    printf("%d\n", pow(a,2));
    // printf("\n");
    // printf("Three nodes in the second tree:\n");
    // for (i=0; i<3; i++) {
        
    //     printf("partialWt =%d, trueWt=%d, flag=%d", pathSecond[i].partialWt, pathSecond[i].partialTrueWt, pathSecond[i].flagWhichTree);
    //     printf("partialAlign is: %s\n", pathSecond[i].partialAlign);
    // }
    
    //test function
    
    int temp;
    temp=trueWt(&pathFirst[1], wtInsertDel, wtSub);
    // printf("test weight is: %d\n", temp);
    //    path[i]=pathTempFirst[k];        // update the candidate paths
    //
    //    struct align pathTempInitial[3];
    //
    //    for (j=0; j<3; j++) {        // make a copy of previous paths
    //        pathTempInitial[j]=path[j];
    //    }
    //
    
    //    if (seqA[0]==seqB[0]) {
    //        tempMatch=0;
    //    }
    //    else tempMatch=1;
    //
    //    struct align path[3]={
    //        {tempMatch*wtSub, "********************", 1, 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "********************", 1, 1, 1, 0, 1},//inserstion
    //        {wtInsertDel, "********************", 1, 1, 0, 1, 1}};//deletion
    
    //    printf("path[0].partialAlign is %s\n", path[0].partialAlign);
    //
    //
    //    //struct align pathTemp[3];
    //    struct align test;
    //    test = path[2];
    //    printf("test!!!!!%s\n", test.partialAlign);
    
    
    
    
    //    struct align path[3]={
    //        {wtSub, "********************", 1, 1, 1, 1, 1},//substitution
    //        {wtInsertDel, "********************", 1, 1, 1, 0, 1},//inserstion
    //        {wtInsertDel, "********************", 1, 1, 0, 1, 1}};//deletion
    //
    //
    //
    //    path[0].partialAlign[0]=seqA[0];
    //    path[0].partialAlign[10]=seqB[0];
    //
    //    path[1].partialAlign[0]=seqA[0];
    //    path[1].partialAlign[10]='-';
    //
    //    path[2].partialAlign[0]='-';
    //    path[2].partialAlign[10]=seqB[0];
    
    //********************************************************************************************************
    //********************************   grow both trees based on initialization ***************************
    //********************************************************************************************************
    
    do{
        
        //        // test output
        //        for (i=0; i<3; i++) {
        //            printf("partial weight of path[%d] is %d\n", i, path[i].partialWt);
        //        }
        //
        //        for (i=0; i<3; i++) {
        //            printf("In path[%d], partial length of A is %d, partial length of B is %d\n", i, path[i].posStringA, path[i].posStringB);
        //            printf("In path[%d], true length of A is %d, true length of B is %d\n", i, path[i].posTrueA, path[i].posTrueB);
        //        }
        
        // printf("\n");
        // printf("Three nodes in the first tree:\n");
        // for (i=0; i<3; i++) {
            
        //     printf("partialWt =%d, trueWt=%d, flag=%d", pathFirst[i].partialWt, pathFirst[i].partialTrueWt, pathFirst[i].flagWhichTree);
        //     printf("partialAlign is: %s  ", pathFirst[i].partialAlign);
        //     printf("position in seqA is:%d ", pathFirst[i].posTrueA);
        //     printf("position in seqB is:%d\n", pathFirst[i].posTrueB);
            
            
        // }
        // printf("\n");
        // printf("Three nodes in the second tree:\n");
        // for (i=0; i<3; i++) {
            
        //     printf("partialWt =%d, trueWt=%d, flag=%d", pathSecond[i].partialWt, pathSecond[i].partialTrueWt, pathSecond[i].flagWhichTree);
        //     printf("partialAlign is: %s  ", pathSecond[i].partialAlign);
        //     printf("position in seqA is:%d ", pathSecond[i].posTrueA);
        //     printf("position in seqB is:%d\n", pathSecond[i].posTrueB);
        // }
        // printf("\n");
        
        
        
        //*************************************** GROW TWO TREES BASED ON TWO METRICS  ******************************************************
        
        
        
        // grow tree according to first order metric: first tree
        
        for (i=0; i<3; i++) {
            
            // printf("%c, %c  ",seqA[pathFirst[i].posTrueA],seqB[pathFirst[i].posTrueB]);
            if(seqA[pathFirst[i].posTrueA] == seqB[pathFirst[i].posTrueB] && pathFirst[i].posTrueA+1<= lengthSeqA && pathFirst[i].posTrueB+1<= lengthSeqB){
                iMatchFirst[i]=0;
                // printf("match or not? : %d\n", iMatchFirst[i]);
            }
            else if (seqA[pathFirst[i].posTrueA] != seqB[pathFirst[i].posTrueB] && pathFirst[i].posTrueA+1<= lengthSeqA && pathFirst[i].posTrueB+1<= lengthSeqB){
                iMatchFirst[i]=1;
                // printf("match or not? : %d\n", iMatchFirst[i]);
            }
            else {
                iMatchFirst[i]=1000;
                // printf("out of length: %d\n", iMatchFirst[i]);
            }
        }
        
        
        int arrayFirst[2][9]= {
            {10, 20, 30, 11, 21, 31, 12, 22, 32},
            {iMatchFirst[0]*wtSub+pathFirst[0].partialWt, wtInsertDel+pathFirst[0].partialWt, wtInsertDel+pathFirst[0].partialWt,
                iMatchFirst[1]*wtSub+pathFirst[1].partialWt, wtInsertDel+pathFirst[1].partialWt, wtInsertDel+pathFirst[1].partialWt,
                iMatchFirst[2]*wtSub+pathFirst[2].partialWt, wtInsertDel+pathFirst[2].partialWt, wtInsertDel+pathFirst[2].partialWt}
        };
        
        
        // grow tree according to second order metric: second tree
        
        
        for (i=0; i<3; i++) {
            
            // printf("%c, %c  ",seqA[pathSecond[i].posTrueA],seqB[pathSecond[i].posTrueB]);
            if(seqA[pathSecond[i].posTrueA] == seqB[pathSecond[i].posTrueB] && pathSecond[i].posTrueA+1<= lengthSeqA && pathSecond[i].posTrueB+1<= lengthSeqB){
                iMatchSecond[i]=0;
                // printf("match or not? : %d\n", iMatchSecond[i]);
            }
            else if (seqA[pathSecond[i].posTrueA] != seqB[pathSecond[i].posTrueB] && pathSecond[i].posTrueA+1<= lengthSeqA && pathSecond[i].posTrueB+1<= lengthSeqB){
                iMatchSecond[i]=1;
                // printf("match or not? : %d\n", iMatchSecond[i]);
            }
            else {
                iMatchSecond[i]=1000;
                // printf("out of length: %d\n", iMatchSecond[i]);
            }
        }
        
        
        int arraySecond[2][9]= {
            {10, 20, 30, 11, 21, 31, 12, 22, 32},
            {iMatchSecond[0]*wtSub*wtSub+pathSecond[0].partialWt, wtInsertDel*wtInsertDel+pathSecond[0].partialWt, wtInsertDel*wtInsertDel+pathSecond[0].partialWt,
                iMatchSecond[1]*wtSub*wtSub+pathSecond[1].partialWt, wtInsertDel*wtInsertDel+pathSecond[1].partialWt, wtInsertDel*wtInsertDel+pathSecond[1].partialWt,
                iMatchSecond[2]*wtSub*wtSub+pathSecond[2].partialWt, wtInsertDel*wtInsertDel+pathSecond[2].partialWt, wtInsertDel*wtInsertDel+pathSecond[2].partialWt}
        };
        
        
        //************************************  SORT TWO TREES  *********************************************************
        
        // IMPORTANT TO BE UPDATED: WHEN THE THREE NODES CONVERGE TO ONE TREE, WE CAN FOCUS ON THAT TREE
        
        // first tree: sorting the second row by bubble and the first row change accordingly
        
        for (c = 0 ; c < ( n - 1 ); c++)
        {
            for (d = 0 ; d < n - c - 1; d++)
            {
                if (arrayFirst[1][d] > arrayFirst[1][d+1]) {
                    swapA = arrayFirst[1][d];
                    arrayFirst[1][d]   = arrayFirst[1][d+1];
                    arrayFirst[1][d+1] = swapA;
                    
                    swapB = arrayFirst[0][d];
                    arrayFirst[0][d]   = arrayFirst[0][d+1];
                    arrayFirst[0][d+1] = swapB;
                    
                }
            }
        }
        
        // second tree: sorting the second row by bubble and the first row change accordingly
        
        for (c = 0 ; c < ( n - 1 ); c++)
        {
            for (d = 0 ; d < n - c - 1; d++)
            {
                if (arraySecond[1][d] > arraySecond[1][d+1]) {
                    swapA = arraySecond[1][d];
                    arraySecond[1][d]   = arraySecond[1][d+1];
                    arraySecond[1][d+1] = swapA;
                    
                    swapB = arraySecond[0][d];
                    arraySecond[0][d]   = arraySecond[0][d+1];
                    arraySecond[0][d+1] = swapB;
                    
                }
            }
        }
        
        
        
        //*************************************** grow first tree by obaining the three good nodes in first tree  ******************
        
        
        for (j=0; j<3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
            // in order to decide whether there is a match or substitution in the next position
            pathTempFirst[j]=pathFirst[j];
        }
        
        for (i=0; i<3; i++){
            
            indicatorFirst = *(*(arrayFirst + 0) + i);   // decide which operation to make
            kFirst = *(*(arrayFirst + 0) + i) % 10;      // decide which path it belongs to
            
            pathFirst[i]=pathTempFirst[kFirst];        // update the candidate paths
            
            // printf("path %d is chosen\n", kFirst);
            // printf("indicator is %d\n", indicatorFirst);
            
            
            if ( indicatorFirst > 9 && indicatorFirst < 15)  // substitution
            {
                
                pathFirst[i].partialWt=pathFirst[i].partialWt+iMatchFirst[kFirst]*wtSub;     // important!! iMatch[k] instead of iMatch[i]
                
                pathFirst[i].partialAlign[pathFirst[i].posStringA]=seqA[pathFirst[i].posTrueA];
                pathFirst[i].partialAlign[10+pathFirst[i].posStringB]=seqB[pathFirst[i].posTrueB];
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                pathFirst[i].posTrueA++;
                pathFirst[i].posTrueB++;
                if (flagEmpty[0]==0) {
                    
                    pathFirst[i].partialTrueWt=trueWt(&pathFirst[i], wtInsertDel, wtSub);
                }
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j=0; j<lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]='-';
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]=seqB[pathFirst[i].posTrueB+j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j=0; j<lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]='-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]=seqA[pathFirst[i].posTrueA+j];
                        }
                    }
                    
                    for (j=0; j<=20; j++) {
                        alignFinal[j]=pathFirst[i].partialAlign[j];
                        
                    }
                    // printf("%s\n", alignFinal);
                    
                    // flagPath[i]=1;
                    flag=1;
                    break;
                }
                
            }
            
            if ( indicatorFirst > 19 && indicatorFirst < 25)    // gap in seqB
            {
                
                pathFirst[i].partialWt=pathFirst[i].partialWt+wtInsertDel;
                pathFirst[i].partialAlign[pathFirst[i].posStringA]=seqA[pathFirst[i].posTrueA];
                pathFirst[i].partialAlign[10+pathFirst[i].posStringB]='-';
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                pathFirst[i].posTrueA++;
                if (flagEmpty[0]==0) {
                    
                    pathFirst[i].partialTrueWt=trueWt(&pathFirst[i], wtInsertDel, wtSub);
                }
                //  path[i].posTrueB++;
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j=0; j<lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]='-';
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]=seqB[pathFirst[i].posTrueB+j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j=0; j<lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]='-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]=seqA[pathFirst[i].posTrueA+j];
                        }
                    }
                    
                    for (j=0; j<=20; j++) {
                        alignFinal[j]=pathFirst[i].partialAlign[j];
                        
                    }
                    // printf("%s\n", alignFinal);
                    
                    //  flagPath[i]=1;
                    flag=1;
                    break;
                }
            }
            
            if ( indicatorFirst > 29 && indicatorFirst < 35)    // gap in seqA
            {
                
                pathFirst[i].partialWt=pathFirst[i].partialWt+wtInsertDel;
                pathFirst[i].partialAlign[pathFirst[i].posStringA]='-';
                pathFirst[i].partialAlign[10+pathFirst[i].posStringB]=seqB[pathFirst[i].posTrueB];
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                // path[i].posTrueA++;
                pathFirst[i].posTrueB++;
                if (flagEmpty[0]==0) {
                    
                    pathFirst[i].partialTrueWt=trueWt(&pathFirst[i], wtInsertDel, wtSub);
                }
                
                // printf("partialTrueWt: %d\n", pathFirst[i].partialTrueWt);
                
                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    
                    
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j=0; j<lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]='-';
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]=seqB[pathFirst[i].posTrueB+j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j=0; j<lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[10+pathFirst[i].posStringB+j]='-';
                            pathFirst[i].partialAlign[pathFirst[i].posStringA+j]=seqA[pathFirst[i].posTrueA+j];
                        }
                    }
                    
                    for (j=0; j<=20; j++) {
                        alignFinal[j]=pathFirst[i].partialAlign[j];
                        
                    }
                    // printf("%s\n", alignFinal);
                    
                    //  flagPath[i]=1;
                    flag=1;
                    break;
                }
            }
        }
        
        //*************************************** grow second tree by obaining the three good nodes in second tree  ******************
        
        if (flag==0) {
            
            
            
            for (j=0; j<3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
                // in order to decide whether there is a match or substitution in the next position
                pathTempSecond[j]=pathSecond[j];
            }
            
            // for (i=0; i<3; i++) {
            //     printf("test one !!! pathSecond[%d].partialTrueWt is: %d\n", i, pathSecond[i].partialTrueWt);
                
            // }
            
            for (i=0; i<3; i++){
                
                indicatorSecond = *(*(arraySecond + 0) + i);   // decide which operation to make
                kSecond = *(*(arraySecond + 0) + i) % 10;      // decide which path it belongs to
                
                pathSecond[i]=pathTempSecond[kSecond];        // update the candidate paths
                
                // printf("path %d is chosen\n", kSecond);
                // printf("indicator is %d\n", indicatorSecond);
                
                
                if ( indicatorSecond > 9 && indicatorSecond < 15)  // substitution
                {
                    
                    pathSecond[i].partialWt=pathSecond[i].partialWt+iMatchSecond[kSecond]*wtSub*wtSub; //Important! iMatch[k] instead ofiMatch[i]
                    
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]=seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[10+pathSecond[i].posStringB]=seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    pathSecond[i].posTrueA++;
                    pathSecond[i].posTrueB++;
                    if (flagEmpty[1]==0) {
                        pathSecond[i].partialTrueWt=trueWt(&pathSecond[i], wtInsertDel, wtSub);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j=0; j<lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]='-';
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]=seqB[pathSecond[i].posTrueB+j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j=0; j<lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]=seqA[pathSecond[i].posTrueA+j];
                            }
                        }
                        
                        for (j=0; j<=20; j++) {
                            alignFinal[j]=pathSecond[i].partialAlign[j];
                            
                        }
                        // printf("%s\n", alignFinal);
                        
                        // flagPath[i]=1;
                        flag=1;
                        break;
                    }
                    
                }
                
                if ( indicatorSecond > 19 && indicatorSecond < 25)    // gap in seqB
                {
                    
                    pathSecond[i].partialWt=pathSecond[i].partialWt+wtInsertDel*wtInsertDel;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]=seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[10+pathSecond[i].posStringB]='-';
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    pathSecond[i].posTrueA++;
                    //  path[i].posTrueB++;
                    if (flagEmpty[1]==0) {
                        pathSecond[i].partialTrueWt=trueWt(&pathSecond[i], wtInsertDel, wtSub);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j=0; j<lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]='-';
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]=seqB[pathSecond[i].posTrueB+j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j=0; j<lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]=seqA[pathSecond[i].posTrueA+j];
                            }
                        }
                        
                        for (j=0; j<=20; j++) {
                            alignFinal[j]=pathSecond[i].partialAlign[j];
                            
                        }
                        // printf("%s\n", alignFinal);
                        
                        //  flagPath[i]=1;
                        flag=1;
                        break;
                    }
                }
                
                if ( indicatorSecond > 29 && indicatorSecond < 35)    // gap in seqA
                {
                    
                    pathSecond[i].partialWt=pathSecond[i].partialWt+wtInsertDel*wtInsertDel;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]='-';
                    pathSecond[i].partialAlign[10+pathSecond[i].posStringB]=seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
                    // path[i].posTrueA++;
                    pathSecond[i].posTrueB++;
                    if (flagEmpty[1]==0) {
                        pathSecond[i].partialTrueWt=trueWt(&pathSecond[i], wtInsertDel, wtSub);
                        
                    }
                    
                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);
                    
                    
                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j=0; j<lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]='-';
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]=seqB[pathSecond[i].posTrueB+j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j=0; j<lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[10+pathSecond[i].posStringB+j]='-';
                                pathSecond[i].partialAlign[pathSecond[i].posStringA+j]=seqA[pathSecond[i].posTrueA+j];
                            }
                        }
                        
                        for (j=0; j<=20; j++) {
                            alignFinal[j]=pathSecond[i].partialAlign[j];
                            
                        }
                        // printf("%s\n", alignFinal);
                        
                        //  flagPath[i]=1;
                        flag=1;
                        break;
                    }
                }
            }
            for (i=0; i<3; i++) {
                // printf("test two !!! pathSecond[%d].partialTrueWt is: %d\n", i, pathSecond[i].partialTrueWt);
                
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
                if (arrayMix[1][d] > arrayMix[1][d+1]) {
                    swapA = arrayMix[1][d];
                    arrayMix[1][d]   = arrayMix[1][d+1];
                    arrayMix[1][d+1] = swapA;
                    
                    swapB = arrayMix[0][d];
                    arrayMix[0][d]   = arrayMix[0][d+1];
                    arrayMix[0][d+1] = swapB;
                    
                }
            }
        }
        
        
        for (i=0; i<3; i++) {
            
            indicatorMix = *(*(arrayMix + 0) + i);   // decide which operation to make
            kMix = *(*(arrayMix + 0) + i) % 10;      // decide which path it belongs to
            
            if (kMix==0 && 9< indicatorMix && indicatorMix <12) {
                path[i]=pathFirst[0];
            }
            else if (kMix==0 && 19< indicatorMix && indicatorMix <22) {
                path[i]=pathFirst[1];
            }
            else if (kMix==0 && 29< indicatorMix && indicatorMix <32) {
                path[i]=pathFirst[2];
            }
            else if (kMix==1 && 9< indicatorMix && indicatorMix <12) {
                path[i]=pathSecond[0];
            }
            else if (kMix==1 && 19< indicatorMix && indicatorMix <22) {
                path[i]=pathSecond[1];
            }
            else path[i]=pathSecond[2];
            
        }
        
        
        
        //****************************************   assign nodes for the next round *************************************************
        
        
        iFirst=0;
        iSecond=0;
        
        
        for (i=0; i<3; i++) {                            // set all six nodes to be infinite nodes
            pathFirst[i]=pathFirstInfinite;
            pathSecond[i]=pathSecondInfinite;
        }
        
        for (i=0; i<3; i++) {                            // assign three candidate nodes to the two trees and other nodes are infinite nodes
            if (path[i].flagWhichTree==1) {
                pathFirst[iFirst]=path[i];
                iFirst++;
            }
            else if(path[i].flagWhichTree==2) {
                pathSecond[iSecond]=path[i];
                iSecond++;
            }
        }
        
        
        
        for (i=0; i<3; i++) {
            if(path[i].flagWhichTree==1){
                flagEmpty[0]=0;
                break;
            }
        }
        for (i=0; i<3; i++) {
            if (path[i].flagWhichTree==2) {
                flagEmpty[1]=0;
                break;
            }
        }
        
        // printf("test three flagEmpty[0]=%d, flagEmpty[1]=%d\n", flagEmpty[0], flagEmpty[1]);
        
        //****************************************   print out tree for testing *************************************************
        
        //        // print first tree for testing
        //        printf("First tree:");
        //        for (i = 0; i < 2; i++) {
        //            for (j = 0; j < 9; j++) {
        //                printf("%d ", arrayFirst[i][j]);
        //            }
        //            printf("\n");
        //        }
        
        
        // print second tree for testing
        //        printf("Second tree:");
        //        for (i = 0; i < 2; i++) {
        //            for (j = 0; j < 9; j++) {
        //                printf("%d ", arraySecond[i][j]);
        //            }
        //            printf("\n");
        //        }
        
        
        
    } while(flag==0);
    
    for(i=0;i<20;i++){
        finalAlign.partialAlign[i]=alignFinal[i];
    }
    
    finalAlign.partialWt=0;
    
    for(i=0;i<10;i++){
        if(finalAlign.partialAlign[i]=='-' || finalAlign.partialAlign[i+10]=='-')
            finalAlign.partialWt=finalAlign.partialWt+wtInsertDel;
        else if(finalAlign.partialAlign[i]==finalAlign.partialAlign[i+10])
            finalAlign.partialWt=finalAlign.partialWt;
        else
        finalAlign.partialWt=finalAlign.partialWt+wtSub;
        
    }
    
    // EDIT: here I'm assigning to retAlign. You might have a better way to do this.
    retAlign->alignment = finalAlign.partialAlign;
    retAlign->weight = finalAlign.partialTrueWt;
    // now, need to free some stuff. I only malloc()ed one thing, so I'm only freeing that, but there will probably need to be more.
    free(initArr);

    // EDIT: returning success code.
    return 0;
}

//****************************************   COMBINE SORT CANDIDATES ACCORDING TO TRUE METRIC  *************************************************

int trueWt(struct align *path, int wtInsertDel, int wtSub){
    
    int i;
    int diff[10]={0,0,0,0,0,0,0,0,0,0};           // difference between two sequences
    // int wtTemp = 0,
    // int wtSub=10;
    // int wtInsertDel=20;
    int wtTempFirst=0, wtTempSecond=0;
    int wtTemp;


    // printf("function value is: %s\n", path->partialAlign);
    // printf("path->posStringA is :%d\n", path->posStringA);

    //   for(j=0;j<3;j++){

    for(i=0; i < path->posStringA ; i++){
        
        if (path->partialAlign[i]=='-' || path->partialAlign[i+10]=='-') {
            diff[i] = wtInsertDel;
        }
        else if (path->partialAlign[i]== path->partialAlign[i+10]) {
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

    // printf("wtTempFirst is: %d\n", wtTempFirst);
    for(i=0; i < path->posStringA ; i++){
        
        wtTempSecond=wtTempSecond+diff[i]*diff[i];
    }

    // printf("wtTempSecond is: %d\n", wtTempSecond);
    wtTemp = wtTempFirst + wtTempSecond;

    // test output

    // printf("wtTemp is:%d\n", wtTemp);
    return 1;

}
