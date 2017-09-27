#ifndef UF_H
#define UF_H

typedef struct
{
    int rank;
    int parent;
    int B, E;
    int handleB, handleE;
} UFelem;

UFelem *UFalloc ( int n );
void UFfree ( UFelem * uf );
void UFcreate ( UFelem * uf, int n );
int UFunion ( UFelem * uf, int i, int j );
int UFfind ( UFelem * uf, int i );

#endif
