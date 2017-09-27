/* $Id: simpleio.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Simple utilities to read and write permutations */

#include <stdio.h>
#include "structs.h"

#ifndef SIMPLEIO_H
#define SIMPLEIO_H

void print_genome ( FILE * outf, char *name, int *genome, int ngenes );
int read3 ( FILE * in, struct genome_struct *gen1, struct genome_struct *gen2,
            struct genome_struct *gen3, int ngenes );
int read_genome ( FILE * in, struct genome_struct *genome, int ngenes );


#endif
