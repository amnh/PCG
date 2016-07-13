//
//  Header.h
//  haskellcode_version
//
//  Created by Yu Xiang on 2/17/16.
//  Copyright © 2016 Yu Xiang. All rights reserved.
//

#ifndef Header_h
#define Header_h

struct align {
    int partialWt;
    int partialTrueWt;
    char partialAlign[20];
    int posStringA;   // position at stringA
    int posStringB;   // position at stringB
    int posTrueA;     // position without gap
    int posTrueB;     // position without gap
    int flagWhichTree;  // belongs to first or second tree
};


int trueWt(struct align *path, int a, int b);

struct align f(char *seq1, char *seq2, int wtInsertDel, int wtSub);


#endif /* Header_h */
