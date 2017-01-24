#include "costMatrix.h"
#include "dynamicCharacterOperations.h"

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
    toReturn->second       = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    toReturn->first        = 0;
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

void freeKeys_t ( const keys_t* toFree) {
    //typedef std::pair<dcElement_t, dcElement_t> keys_t;
    freeDCElem(&toFree->first);
    freeDCElem(&toFree->second);
    // free(&toFree);
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
        freeKeys_t(&thing.first);
    }
    myMatrix.clear();
    //hasher.clear();

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
    keys_t toLookup;
    toLookup.first  = *left;
    toLookup.second = *right;
    mapIterator found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        if(DEBUG) printf("\ngetSetCost didn't find %llu %llu.\n", left->element[0], right->element[0]);
        costMedian_t* computedCostMed = computeCostMedian(toLookup);
        if(DEBUG) printf("computed cost, median: %2i %llu\n", computedCostMed->first, computedCostMed->second[0]);
        foundCost                     = computedCostMed->first;
        copyPackedChar(computedCostMed->second, retMedian->element, alphabetSize);

        setValue (toLookup, computedCostMed);
    } else {
            // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost          = found->second.first;
        retMedian->element = found->second.second;
    }

    return foundCost;
}

costMedian_t* CostMatrix::computeCostMedian(keys_t key) {
    int curCost                    = 0;                               // don't actually need to initialize this
    dcElement_t*  firstUnambigKey  = allocateDCElement(alphabetSize); // these will get set and used for lookup
    dcElement_t*  secondUnambigKey = allocateDCElement(alphabetSize);
    packedChar*   curMedian        = (packedChar*) calloc(dynCharSize(alphabetSize, 1), INT_WIDTH);
    packedChar*   interimMedian;     // this will be allocated as it's used
    costMedian_t* toReturn         = allocCostMedian_t(key.first.alphSize); // not allocating second,
                                                                            // because will be alloc'ed in curMedian
    toReturn->first                = INT_MAX;

    firstUnambigKey->alphSize      = alphabetSize;
    secondUnambigKey->alphSize     = alphabetSize;
    keys_t* searchKey              = allocKeys_t(alphabetSize);
    searchKey->first               = *firstUnambigKey;
    searchKey->second              = *secondUnambigKey;

    mapIterator found;

    // if(DEBUG) {
    //     for ( auto& thing: myMatrix ) {
    //         printf("%2llu %2llu\n", thing.first.first.element[0], thing.first.second.element[0]);
    //         //printElemBits(&thing.first.first);
    //         //printElemBits(&thing.first.second);
    //     }
    // }

    for (uint64_t posFirstKey = 0; posFirstKey < alphabetSize; ++posFirstKey) {
        if( TestBit(key.first.element, posFirstKey) ) {
            printf("pos1: %llu\n", posFirstKey);
            SetBit(searchKey->first.element, posFirstKey);
            for (uint64_t posSecondKey = 0; posSecondKey < alphabetSize; ++posSecondKey) {
                if( TestBit(key.second.element, posSecondKey) ) {
                    printf("pos2: %llu\n", posSecondKey);
                    SetBit(searchKey->second.element, posSecondKey);
                    // free(found.second);
                    found = myMatrix.find(*searchKey);
                    if ( found == myMatrix.end() ) {
                        printf("Something went wrong in cost matrix.\n");
                        printf("missing key: %llu %llu\n", *searchKey->first.element, *searchKey->second.element);
                        exit(1);
                    }
                    printf("curMin: %i curCost: %i, foundCost: %i\n", toReturn->first, curCost, found->second.first);
                    curCost = found->second.first;

                    // now seemingly recreating logic in CM_distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost < toReturn->first) {
                        if(DEBUG) {
                            printf("\nNew low cost.\n");
                            printf("keys: %llu %llu\n", *searchKey->first.element, *searchKey->second.element);
                            printf("current median:  %llu\n", found->second.second[0]);
                        }
                        toReturn->first = curCost;
                        copyPackedChar(found->second.second, curMedian, alphabetSize);
                        if(DEBUG) {
                            printf("returned median: %llu\n", found->second.second[0]);
                            printf("found cost:      %d\n",   found->second.first);
                        }
                    } else if (curCost == toReturn->first) {
                        if(DEBUG) {
                            printf("\nSame cost, new median.\n");
                            printf("keys: %llu %llu\n", *searchKey->first.element, *searchKey->second.element);
                            printf("returned median: %llu\n", found->second.second[0]);
                            printf("found cost:      %d\n", found->second.first);
                        }
                        interimMedian = packedCharOr(curMedian, found->second.second, alphabetSize);
                        free(curMedian); // because curMedian will get allocation from interinMedian, which was just allocated.
                        curMedian = interimMedian;
                        printf("new median: %llu\n", *curMedian);
                    }
                    ClearBit(searchKey->second.element, posSecondKey);
                } // if pos2 is set in second key
            } // posSecondKey
            ClearBit(searchKey->first.element, posFirstKey);
        } // if pos1 is set in first key
    } // posFirstKey
    // freeDCElem(firstUnambigKey);
    // freeDCElem(secondUnambigKey);
    // freeKeys_t(searchKey);
    copyPackedChar( curMedian, toReturn->second, alphabetSize );
    return toReturn;
}

costMedian_t* CostMatrix::findDistance (keys_t& key, int* tcm) {

    packedChar* key1       = key.first.element;
    packedChar* key2       = key.second.element;
    size_t dynCharLen      = dcElemSize(alphabetSize);
    packedChar* curMedian  = (packedChar*)  calloc( dynCharLen, INT_WIDTH );
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    toReturn->second       = curMedian;
    int curMin             = INT_MAX;
    int curCost            = 0;

    for (size_t pos1 = 0; pos1 < alphabetSize; pos1++) {
        if ( TestBit(key1, pos1) ) { // if pos1 is possible value of key1
            for (size_t pos2; pos2 < alphabetSize; pos2++) {
                if ( TestBit(key2, pos2) ) { // pos1 in key1 and pos2 in key2
                    curCost = tcm[pos1* alphabetSize + pos2];
                    if (curCost < curMin) {
                        curMin = curCost;
                        ClearAll (curMedian, dynCharLen);
                        SetBit (curMedian, pos1);
                        SetBit (curMedian, pos2);
                    } else if (curCost == curMin) {
                        SetBit (curMedian, pos1);
                        SetBit (curMedian, pos2);
                    }
                } // pos2 in key2
            } // pos2
        } // pos1 in key1
    } // key1
    free(key1);
    free(key2);
    //free(curMedian);
    toReturn->first = curMin;
    return toReturn;
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

            SetBit(firstKey->element, key1);
            SetBit(median, key1);

            SetBit(secondKey->element, key2);
            SetBit(median, key2);

            *cost = tcm[key1 * alphabetSize + key2];

            // if(DEBUG) {
            //     printf("keys set: %llu %llu\n", *firstKey->element, *secondKey->element);
            //     printf("median: %llu   cost: %i\n", *toInsert->second.second, toInsert->second.first);
            // }

            myMatrix.insert(*toInsert);

        } // key2
    }
    // printf("finished initializing\n");
    // printf("freed keys\n");
}

void CostMatrix::setValue(keys_t key, costMedian_t* median) {
    myMatrix.insert(std::make_pair(key, *median));
}