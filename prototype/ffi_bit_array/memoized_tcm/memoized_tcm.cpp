// #include <cstdint>
// #include "memoized_tcm.h"
// using namespace std;

// int main()
// {
//     Matrix x;
//     x["one"] = 1;
//     x["two"] = 2;
//     x["three"] = 3;

//     assert(x.at("one") == 1);
//     assert(x.find("missing") == x.end());

// }

#include <cstdint>
#import <pair>
#include "dynamicCharacterOperations.h"
#include "memoized_tcm.h"
using namespace std;

int main() {
    MemoizedTcm x;
    
}

MemoizedTCM( double *costArray, size_t alphSize ) {

    for( size_t aIdx = 0; aIdx < alphSize; aIdx++ ) {
        for( size_t bIdx = 0; bIdx < alphSize; bIdx++ ) {
            costMatrix[] = costArray[aIdx + bIdx];
        }
    }
}

returnType getCost( dynChar_t const &charA, dynChar_t const &charB) {
    int64_t xored = independentVal;
    for( int i = 0; i < length; i++ ) {
        xored ^= charA[i] ^ charB[i];
    }
    if( constMatrix.at(xored) ) {
        return costMatrix.find(xored) 
    } else {
        costMatrix[xored] = calculateCost(charA, charB, length);
    }
    return costMatrix[xored];
}

returnType calculateCost( dynChar_t const *charA, dynChar_t const *charB) {
    dynChar_t charAcc
    size_t aIdx = 0, bIdx = 0;
    double scoreAcc = 0;

    for( ; aIdx < charA -> alphSize; aIdx++ ) {
        for( ; bIdx < charA -> alphSize; bIdx++ ) {
            if( TestBit(seqA, aIdx) && TestBit(seqB, bIdx) ) {
                

            }
        }
    }
}

typedef struct dynChar_t {
    unsigned int alphSize;
    unsigned int dynCharLen;
    uint64_t* dynChar;
} dynChar_t;