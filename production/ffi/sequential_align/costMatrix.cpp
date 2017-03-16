#include <inttypes.h>

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
#include <cstring> //for memcpy;

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

costMatrix_p construct_CostMatrix_C(size_t alphSize, int* tcm) {
    return new CostMatrix(alphSize, tcm);
}

void destruct_CostMatrix_C(costMatrix_p untyped_self) {
    delete static_cast<CostMatrix*> (untyped_self);
}

int call_getSetCost_C(costMatrix_p untyped_self, dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {

    CostMatrix* thisMtx = static_cast<CostMatrix*> (untyped_self);
    return thisMtx->getSetCostMedian(left, right, retMedian);
}


// costMatrix_p get_CostMatrixPtr_C(costMatrix_p untyped_self);


costMedian_t* allocCostMedian_t (size_t alphabetSize) {
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    toReturn->first        = 0;
    toReturn->second       = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    return toReturn;
}

void freeCostMedian_t (costMedian_t* toFree) {
    free(toFree->second);
    // free(toFree);
}

keys_t* allocKeys_t (size_t alphSize) {
    keys_t* toReturn   = (keys_t*) malloc(sizeof(std::pair<dcElement_t, dcElement_t>));

    dcElement_t* left  = allocateDCElement(alphSize);
    dcElement_t* right = allocateDCElement(alphSize);
    left->alphSize     = right->alphSize = alphSize;

    toReturn->first    = *left;
    toReturn->second   = *right;

    return toReturn;
}

// TODO: since keys_t is Pair<dcElement_t, dcElement_t>, there are no pointers, and nothing
// to free?? How is this right?
void freeKeys_t ( const keys_t* toFree) {
    freeDCElem(&toFree->first);
    freeDCElem(&toFree->second);
}

mapAccessPair_t* allocateMapAccessPair (size_t alphSize) {
    mapAccessPair_t* toReturn;
    toReturn = (mapAccessPair_t*) malloc( sizeof(mapAccessPair_t) );
    toReturn->first  = *allocKeys_t(alphSize);
    toReturn->second = *allocCostMedian_t(alphSize);
    return toReturn;
}


CostMatrix::CostMatrix(size_t alphSize, int* tcm) {
    alphabetSize = alphSize;
    setUpInitialMatrix(tcm);
}

CostMatrix::~CostMatrix() {
    for ( auto& thing: myMatrix ) {
    // for ( mapIterator thing = myMatrix.begin(); thing != myMatrix.end(); thing++ ) {
        freeCostMedian_t(&thing.second);

        // TODO: since keys_t is Pair<dcElement_t, dcElement_t>, there are no pointers, and nothing
        // to free?? How is this right? Anyway, skipping next line.
        // freeKeys_t(&thing.first);
    }
    myMatrix.clear();
    hasher.clear();

}

int CostMatrix::getCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {
    keys_t toLookup;
    toLookup.first  = *left;
    toLookup.second = *right;
    mapIterator found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        foundCost          = found->second.first;
        retMedian->element = found->second.second;
    }

    return foundCost;
}

int CostMatrix::getSetCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {
    // not using allocKeys_t because we're making a copy of the packed characters _if we need to_
    keys_t* toLookup = (keys_t*) malloc( sizeof(keys_t) );
    toLookup->first  = *left;
    toLookup->second = *right;
    mapIterator found;
    int foundCost;

    // printf("1st: {%zu}: %" PRIu64 "\n", toLookup->first.alphSize , *toLookup->first.element );
    // printf("2nd: {%zu}: %" PRIu64 "\n", toLookup->second.alphSize, *toLookup->second.element);

    found = myMatrix.find(*toLookup);

    // if (retMedian->element == NULL) {
    //     retMedian->element = (packedChar*) calloc( dcElemSize(alphabetSize), sizeof(packedChar) );
    // }

    if ( found == myMatrix.end() ) {
        if(DEBUG) printf("\ngetSetCost didn't find %" PRIu64 " %" PRIu64 ".\n", left->element[0], right->element[0]);

        // costMedian_t* computedCostMed = computeCostMedian(*toLookup);
        costMedian_t* computedCostMed = computeCostMedianFitchy(*toLookup);

        if(DEBUG) printf("computed cost, median: %2i %" PRIu64 "\n", computedCostMed->first, computedCostMed->second[0]);

        foundCost          = computedCostMed->first;
        retMedian->element = makePackedCharCopy( computedCostMed->second, alphabetSize, 1 );

        // Can't use allocateDCElement here, because makePackedCharCopy() allocates.
        toLookup->first           = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        toLookup->first.alphSize  = left->alphSize;
        toLookup->first.element   = makePackedCharCopy(left->element, left->alphSize, 1);
        toLookup->second          = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        toLookup->second.alphSize = left->alphSize;
        toLookup->second.element  = makePackedCharCopy( right->element, right->alphSize, 1 );

        setValue (toLookup, computedCostMed);
    } else {
        // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost          = found->second.first;
        retMedian->element = makePackedCharCopy( found->second.second, left->alphSize, 1 );
    }

    return foundCost;
}

costMedian_t* CostMatrix::computeCostMedian(keys_t keys) {
    int curCost;
    int minCost = INT_MAX;
    size_t elemArrLen = dcElemSize(alphabetSize);

    packedChar* median = (packedChar*) calloc( elemArrLen, sizeof(uint64_t) );

    dcElement_t*  firstKey         = &keys.first;
    dcElement_t*  secondKey        = &keys.second;
    // dcElement_t*  singleNucleotide = (dcElement_t*) malloc(sizeof(dcElement_t));
    packedChar*   curMedian        = (packedChar*) calloc(elemArrLen, INT_WIDTH);  // don't free, it's going into toReturn
    costMedian_t* toReturn         = (costMedian_t*) malloc(sizeof(costMedian_t)); // array is alloc'ed above

    keys_t* searchKey              = allocKeys_t(alphabetSize);

    dcElement_t* singleNucleotide = &searchKey->second;

    // if(DEBUG) {
    //     for ( auto& thing: myMatrix ) {
    //         printf("%2llu %2llu\n", thing.first.first.element[0], thing.first.second.element[0]);
    //         //printElemBits(&thing.first.first);
    //         //printElemBits(&thing.first.second);
    //     }
    // }


    curCost  = INT_MAX;
    for (size_t curNucleotideIdx = 0; curNucleotideIdx < alphabetSize; ++curNucleotideIdx) {
        SetBit(singleNucleotide->element, curNucleotideIdx);
        curCost = findDistance(searchKey, firstKey)
                + findDistance(searchKey, secondKey);

        // now seemingly recreating logic in findDistance(). However, that was to get the cost for the
        // ambElem on each child; now we're combining those costs get overall cost and median
        if (curCost < minCost) {
            // printf("\nNew low cost.\n");
            // printf("current nucleotide: %" PRIu64 " \n", *searchKey->second.element);
            // printf("found cost:      %d\n", curCost);

            minCost = curCost;
            ClearAll(curMedian, elemArrLen);
            SetBit(curMedian, curNucleotideIdx);
        } else if (curCost == minCost) {
            // printf("\nSame cost, new median.\n");
            // printf("current nucleotide: %" PRIu64 " \n", *searchKey->second.element);
            // printf("median: %" PRIu64 "\n", *curMedian);
            // printf("found cost:      %d\n", curCost);

            SetBit(curMedian, curNucleotideIdx);

            // printf("new median: %" PRIu64 "\n", *curMedian);
        }
        ClearBit(singleNucleotide->element, curNucleotideIdx);
    } // curNucleotideIdx
    toReturn->first  = minCost;
    toReturn->second = curMedian;

    freeKeys_t(searchKey);
    free(searchKey);
    // freeDCElem(singleNucleotide);

    return toReturn;
}

costMedian_t* CostMatrix::computeCostMedianFitchy(keys_t keys) {

    size_t elemArrLen = dcElemSize(alphabetSize);

    dcElement_t*  firstUnambiguous  = allocateDCElement(alphabetSize);
    dcElement_t* secondUnambiguous  = allocateDCElement(alphabetSize);
    dcElement_t*    pointwiseMedian = allocateDCElement(alphabetSize);
    dcElement_t*      minimalMedian = allocateDCElement(alphabetSize);

    int minimalCost = INT_MAX;
    int pointwiseCost;
    for (size_t i = 0; i < alphabetSize; ++i) {
      if (TestBit(keys.first.element, i)) {
        SetBit(firstUnambiguous->element, i);
        for (size_t j = 0; j < alphabetSize; ++j) {
           if (TestBit(keys.second.element, j)) {
             SetBit(secondUnambiguous->element, j);
             // printf("secondUnambiguous: %" PRIu64 "\n", *secondUnambiguous->element);
             pointwiseCost = getSetCostMedian(firstUnambiguous, secondUnambiguous, pointwiseMedian);
             // printf("bit1: %zu, bit2: %zu, cost: %d\n", i, j, pointwiseCost);
             if (pointwiseCost == minimalCost) {
               // printf("union\n");
               // printf("old median: %" PRIu64 "\n", *minimalMedian->element);
               // printf("secondUnambiguous: %" PRIu64 "\n", *secondUnambiguous->element);
               SetBit(minimalMedian->element, i);
               SetBit(minimalMedian->element, j);
               //packedChar *newPackedChar = packedCharOr(secondUnambiguous->element, minimalMedian->element, alphabetSize);
               //free(minimalMedian->element);
               //minimalMedian->element = newPackedChar;
               // printf("new element: %" PRIu64 "\n", *minimalMedian->element);
             } else if (pointwiseCost < minimalCost) {
                 // printf("new min\n");
               //memcpy(minimalMedian->element, pointwiseMedian->element, elemArrLen * sizeof(*pointwiseMedian->element));
               memset(minimalMedian->element, 0, elemArrLen);
               SetBit(minimalMedian->element, i);
               SetBit(minimalMedian->element, j);
               minimalCost = pointwiseCost;
               // printf("new element: %" PRIu64 "\n", *minimalMedian->element);
             } else {
                 // printf("discarded\n");
               // Greater than current minimal cost, don't accumulate.
               // This is hear because Alex is crazy. "So you know it's intentional, you know, for completion!"
             }
             ClearBit(secondUnambiguous->element, j);
           }
        }
        ClearBit(firstUnambiguous->element, i);
      }
    }

    freeDCElem( firstUnambiguous);
    freeDCElem(secondUnambiguous);
    freeDCElem(   pointwiseMedian);

    costMedian_t* toReturn = (costMedian_t*) malloc(sizeof(costMedian_t));

    toReturn->first  = minimalCost;
    toReturn->second = minimalMedian->element;

    free(minimalMedian);

    return toReturn;
}

/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
int CostMatrix::findDistance (keys_t* searchKey, dcElement_t* ambElem) {
    mapIterator found;
    int min = INT_MAX;
    int curCost;
    for (size_t pos = 0; pos < alphabetSize; pos++) {
        if ( TestBit(ambElem->element, pos) ) {
            SetBit(searchKey->first.element, pos);
            found = myMatrix.find(*searchKey);
            if ( found == myMatrix.end() ) {
                printf("Something went wrong in the memoized cost matrix.\n");
                printf("missing key: %" PRIu64 " %" PRIu64 "\n", *searchKey->first.element, *searchKey->second.element);
                exit(1);
            }
            curCost = found->second.first;
            if (curCost < min) {
                min = curCost;
            }
            ClearBit(searchKey->first.element, pos);
        }
    }
    // printf("distance ambElem: %" PRIu64 ", nucleotide: %" PRIu64 "\n", ambElem->element[0], *searchKey->second.element);
    // printf("cost: %i\n", min);
    return min;
}


void CostMatrix::setUpInitialMatrix (int* tcm) {
    keys_t* keys;
    costMedian_t* costMedian;
    mapAccessPair_t* toInsert;

    dcElement_t* firstKey;
    dcElement_t* secondKey;
    packedChar* median;
    int* cost;

    for (size_t key1 = 0; key1 < alphabetSize; key1++) { // for every possible value of key1, key2

        // key2 starts from 0, so non-symmetric matrices should work
        for (size_t key2 = 0; key2 < alphabetSize; key2++) { // doesn't assumes 0 diagonal

            toInsert = allocateMapAccessPair(alphabetSize);

            firstKey  = &toInsert->first.first;
            secondKey = &toInsert->first.second;
            median    = toInsert->second.second;
            cost      = &toInsert->second.first;

            // we allocated a new pair above, so we never will clear the bits set here.
            SetBit(firstKey->element, key1);
            SetBit(median, key1);

            SetBit(secondKey->element, key2);
            SetBit(median, key2);

            *cost = tcm[key1 * alphabetSize + key2];

            // if(DEBUG) {
            //     printf("keys set: %" PRIu64 " %" PRIu64 "\n", *firstKey->element, *secondKey->element);
            //     printf("median: %" PRIu64 "   cost: %i\n", *toInsert->second.second, toInsert->second.first);
            // }

            myMatrix.insert(*toInsert);

        } // key2
    }
    // printf("finished initializing\n");
    // printf("freed keys\n");
}

void CostMatrix::setValue(keys_t* key, costMedian_t* median) {
    myMatrix.insert(std::make_pair(*key, *median));
}
