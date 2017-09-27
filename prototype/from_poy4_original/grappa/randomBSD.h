#ifndef RANDOMBSD_H
#define RANDOMBSD_H

#ifdef CYGWINNT
#ifndef GCC
#define u_int unsigned int

void srandom ( u_int x );
char *initstate ( u_int seed, char *arg_state, int n );
char *setstate ( char *arg_state );
long random (  );

#endif
#endif

#endif
