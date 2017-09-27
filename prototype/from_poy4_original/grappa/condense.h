#ifndef CONDENSE_H
#define CONDENSE_H

#include "structs.h"

void condense_genes ( struct genome_struct *genome_list,
                      int num_genomes, int *num_genes, int CIRCULAR,
                      int *succ, int *decode );

void print_tree_genomes_uncondensed ( struct tNode *tree, int num_genes,
                                      int *condense_succ,
                                      int *condense_decode,
                                      int orig_num_genes );

void print_tree_nexus ( struct tNode *tree );
void print_tree_nexus_noscore ( char *TreeString, struct tNode *tree );
void print_tree_uncondensed ( struct tNode *tree, int num_genes,
                              int *condense_succ, int *condense_decode,
                              int orig_num_genes );

#endif
