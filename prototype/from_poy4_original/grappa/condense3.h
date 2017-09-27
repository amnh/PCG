#ifndef CONDENSE3_H
#define CONDENSE3_H

void condense3 ( int *ingene1, int *ingene2, int *ingene3,
                 int *outgene1, int *outgene2, int *outgene3,
                 int num_genes, int *num_cond,
                 int *succ, int *pred, int *code, int *decode );

void decode3 ( int *outgenes, int *ingenes, int *succ, int *decode,
               int num_cond );

#endif
