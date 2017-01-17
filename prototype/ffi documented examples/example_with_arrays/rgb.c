// from http://stackoverflow.com/questions/35050825/haskell-ffi-passing-array-of-c-structs-in-and-out

#include <stdlib.h>
#include <stdio.h>
#include "rgb.h"

void rgb_test(rgb_t * rgbs, ssize_t n)
{
    int i;

    for(i=0; i<n; i++) {
        printf("%.3f %.3f %.3f\n", rgbs[i].r, rgbs[i].g, rgbs[i].b);
        rgbs[i].r *= 2.0;
        rgbs[i].g *= 2.0;
        rgbs[i].b *= 2.0;
    }
}