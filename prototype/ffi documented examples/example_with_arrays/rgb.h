// from http://stackoverflow.com/questions/35050825/haskell-ffi-passing-array-of-c-structs-in-and-out

#ifndef RGB_H
#define RGB_H

#include <stdlib.h>

typedef struct {
    float r;
    float g;
    float b;
} rgb_t;

void rgb_test(rgb_t * rgbs, ssize_t n);

#endif

