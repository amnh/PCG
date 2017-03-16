#include "costMatrix.h"
#include "dynamicCharacterOperations.h"

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

costMedian_t* allocCostMedian_t (size_t alphabetSize) {
    costMedian_t *toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    toReturn->second       = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    toReturn->first        = 0;
    return toReturn;
}

void freeCostMedian_t (costMedian_t* toFree) {
    free(toFree->second);
    free(toFree);
}

keys_t* allocKeys_t (size_t alphSize) {
    keys_t *toReturn   = (keys_t*) malloc(sizeof(std::pair<dcElement_t, dcElement_t>));

    dcElement_t *left  = allocateDCElement(alphSize);
    dcElement_t *right = allocateDCElement(alphSize);
    left->alphSize     = right->alphSize = alphSize;

    toReturn->first    = *left;
    toReturn->second   = *right;

    return toReturn;
}

void freeKeys_t (keys_t *toFree) {
    typedef std::pair<dcElement_t, dcElement_t> keys_t;
    freeDCElem(&toFree->first);
    freeDCElem(&toFree->second);
    free(toFree);
}

mapAccessPair_t* allocateMapAccessPair (size_t alphSize) {
    mapAccessPair_t *toReturn;
    toReturn = (mapAccessPair_t*) malloc( sizeof(mapAccessPair_t) );
    toReturn->first  = *allocKeys_t(alphSize);
    toReturn->second = *allocCostMedian_t(alphSize);
    return toReturn;
}

costMatrix_t matrixInit(size_t alphSize, int *tcm) {
   return new CostMatrix(alphSize, tcm);
}

void matrixDestroy(costMatrix_t *untyped_ptr) {
    CostMatrix* typed_ptr = static_cast<CostMatrix*>(untyped_ptr);
    typed_ptr::~CostMatrix();  //TODO:
}

int getCost(costMatrix_t untyped_self, dcElement_t *left, dcElement_t *right, dcElement_t *retMedian) {
    CostMatrix* typed_self = static_cast<CostMatrix*> (untyped_self);
    return typed_self->getSetCost(left, right, retMedian);
}

// CostMatrix::CostMatrix() {
//     alphabetSize = 2;

// }

CostMatrix::CostMatrix(size_t alphSize, int *tcm) {
    alphabetSize = alphSize;
    setUpInitialMatrix(tcm);
}

CostMatrix::~CostMatrix() {
    for ( auto& thing: myMatrix) {
    // for ( auto thing = myMatrix.begin(); thing != myMatrix.end(); thing++ ) {
        freeCostMedian_t(thing.second);
        freeKeys_t(thing.first);
    }
    myMatrix.clear();

}

int CostMatrix::getSetCost(dcElement_t *left, dcElement_t *right, dcElement_t *retMedian) {
    keys_t toLookup;
    toLookup.first  = *left;
    toLookup.second = *right;
    mapIterator found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        printf("nope.\n");
        costMedian_t* computedCostMed = computeCostMedian(toLookup);
        foundCost                     = computedCostMed->first;
        retMedian->element            = computedCostMed->second;

        setValue (toLookup, computedCostMed);
    } else {
            // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost          = found->second.first;
        retMedian->element = found->second.second;
    }

    return foundCost;
}

costMedian_t* CostMatrix::computeCostMedian(keys_t key) {
    int curCost                   = 0; // don't actually need to initialize this
    dcElement_t* firstUnambigKey  = allocateDCElement(alphabetSize); // these will get set and used for lookup
    dcElement_t* secondUnambigKey = allocateDCElement(alphabetSize);
    packedChar *curMedian         = (packedChar*) calloc(dynCharSize(alphabetSize, 1), INT_WIDTH);
    packedChar *interimMedian     = (packedChar*) calloc(dynCharSize(alphabetSize, 1), INT_WIDTH);

    costMedian_t* toReturn = allocCostMedian_t(key.first.alphSize); // not allocating second, because will be alloc'ed in curMedian
    toReturn->first        = INT_MAX;

    firstUnambigKey->alphSize  = alphabetSize;
    secondUnambigKey->alphSize = alphabetSize;
    keys_t* searchKey          = allocKeys_t(alphabetSize);
    searchKey->first           = *firstUnambigKey;
    searchKey->second          = *secondUnambigKey;

    mapIterator found;

    for (uint64_t posFirstKey = 1; posFirstKey <= alphabetSize; posFirstKey++) {
        if( TestBit(key.first.element, posFirstKey) ) {
            for (uint64_t posSecondKey = 1; posSecondKey <= alphabetSize; posSecondKey++) {
                if( TestBit(key.second.element, posSecondKey) ) {
                    SetBit(searchKey->first.element, posFirstKey);
                    SetBit(searchKey->second.element, posFirstKey);
                    // free(found.second);
                    found   = myMatrix.find(key);  //TODO: should I have failure to find case here?
                    curCost = found->second.first;
                    // now seemingly recreating logic in CM_distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost < toReturn->first) {
                        toReturn->first = curCost;
                        free(curMedian);
                        curMedian = found->second.second; // TODO: this is not making a copy, so need to fix it.
                    } else if (curCost == toReturn->first) {
                        interimMedian = packedCharOr(curMedian, found->second.second, alphabetSize);
                        free(curMedian);
                        curMedian = interimMedian;
                        free(interimMedian);
                    }
                } // if pos2 is set in second key
            } // if pos1 is set in first key
        } // posSecondKey
    } // posFirstKey
    // freeDCElem(firstUnambigKey);
    // freeDCElem(secondUnambigKey);
    // freeKeys_t(searchKey);
    return toReturn;
}

costMedian_t* CostMatrix::findDistance (keys_t &key, int *tcm) {

    packedChar *key1      = key.first.element;
    packedChar *key2      = key.second.element;
    size_t dynCharLen      = dcElemSize(alphabetSize);
    packedChar *curMedian = (packedChar*)  calloc( dynCharLen, INT_WIDTH );
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    toReturn->second    = curMedian;
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


void CostMatrix::setUpInitialMatrix (int *tcm) {
    keys_t *keys;
    costMedian_t *costMedian;
    mapAccessPair_t *toInsert;

    dcElement_t *firstKey;
    dcElement_t *secondKey;
    packedChar *median;
    int* cost;

    // mapIterator found;

    //firstKey->element   = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    //secondKey->element  = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    // firstKey->alphSize  = alphabetSize;
    // secondKey->alphSize = alphabetSize;
    //median              = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    // median->alphSize    = alphabetSize;
    //cost                = (int*) malloc(sizeof(int));
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

            // printf("key 1 set: %zu\n", key1);
            // printf("key 2 set: %zu\n", key2);

            // printf("median:\n");

            // printPackedChar(median, 1, alphabetSize);

            // printPackedChar(median, 1, alphabetSize);
            myMatrix.insert(*toInsert); //.second
            // if ( !myMatrix.insert(*toInsert).second ) {
            //     // printf("Out of memory or collision.\n");
            //     printf("failed to insert!!\n");
            //     printf("first key:\n");
            //     printPackedChar(firstKey->element, 1, alphabetSize);
            //     printf("second key:\n");
            //     printPackedChar(secondKey->element, 1, alphabetSize);
            //     printf("median:\n");
            //     printPackedChar(median, 1, alphabetSize);
            //     exit (1);
            // }
            // found = myMatrix.find(toInsert->first);
            // printf("found median:\n");
            // printPackedChar (found->second.second, 1, alphabetSize);


        } // key2
    }
    // printf("finished initializing\n");
    // printf("freed keys\n");
}

void CostMatrix::setValue(keys_t key, costMedian_t *median) {
    myMatrix.insert(std::make_pair(key, *median));
}