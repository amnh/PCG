#ifndef READ_INPUT_H
#define READ_INPUT_H

#include "structs.h"

void check_genes ( int *genes, int num_genes, int at_genome );

void read_data ( FILE * input, struct genome_struct **genome_list,
                 int *num_genes, int *num_genomes, int INVDIST, int CIRCULAR,
                 int *condense_succ, int *condense_decode,
                 int *orig_num_genes );


#endif
