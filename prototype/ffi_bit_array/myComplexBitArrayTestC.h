#include <stdint.h>

struct alignResult_t {
    int finalWt;
    int finalLength;
    uint64_t* finalStr;
};

struct dynChar_t {
    int alphSize;
    int dynCharLen;
    uint64_t* dynChar;
};

int testFN(struct dynChar_t*, struct dynChar_t*, struct alignResult_t*);