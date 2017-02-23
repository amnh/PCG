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
            // printf("current nucleotide: %llu \n", *searchKey->second.element);
            // printf("found cost:      %d\n", curCost);

            minCost = curCost;
            ClearAll(curMedian, elemArrLen);
            SetBit(curMedian, curNucleotideIdx);
        } else if (curCost == minCost) {
            // printf("\nSame cost, new median.\n");
            // printf("current nucleotide: %llu \n", *searchKey->second.element);
            // printf("median: %llu\n", *curMedian);
            // printf("found cost:      %d\n", curCost);

            SetBit(curMedian, curNucleotideIdx);

            // printf("new median: %llu\n", *curMedian);
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
                printf("missing key: %llu %llu\n", *searchKey->first.element, *searchKey->second.element);
                exit(1);
            }
            curCost = found->second.first;
            if (curCost < min) {
                min = curCost;
            }
            ClearBit(searchKey->first.element, pos);
        }
    }
    // printf("distance ambElem: %llu, nucleotide: %llu\n", ambElem->element[0], *searchKey->second.element);
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