#include <inttypes.h>

#include "costMatrix.h"
#include "costMatrix_3d.h"
#include "dynamicCharacterOperations.h"
#include <cstring> //for memcpy;

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html


costMatrix_p construct_CostMatrix_3d_C( size_t alphSize, int* tcm )
{
    return new CostMatrix_3d( alphSize, tcm );
}


void destruct_CostMatrix_3d_C( costMatrix_p untyped_self )
{
    delete static_cast<CostMatrix_3d*> (untyped_self);
}


// TODO: this needs to do some Sankoff stuff, no?
int call_getSetCost_3d_C ( costMatrix_p untyped_self
                         , dcElement_t* first
                         , dcElement_t* second
                         , dcElement_t* third
                         , dcElement_t* retMedian
                         )
{
    CostMatrix_3d* thisMtx = static_cast<CostMatrix_3d*> (untyped_self);
    return thisMtx->getSetCostMedian(first, second, third, retMedian);
}


costMedian_t* allocCostMedian_t (size_t alphabetSize)
{
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    std::get<0>(*toReturn)  = 0;
    std::get<1>(*toReturn)  = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    return toReturn;
}


void freeCostMedian_t (costMedian_t* toFree)
{
    free(std::get<1>(*toFree));
    // free(toFree);
}


keys_3d_t* allockeys_3d_t (size_t alphSize)
{
    keys_3d_t* toReturn = (keys_3d_t*) malloc(sizeof(std::tuple<dcElement_t, dcElement_t, dcElement_t>));

    dcElement_t* first   = allocateDCElement(alphSize);
    dcElement_t* second  = allocateDCElement(alphSize);
    dcElement_t* third   = allocateDCElement(alphSize);

    first->alphSize = second->alphSize = third->alphSize = alphSize;

    std::get<0>(*toReturn) = *first;
    std::get<1>(*toReturn) = *second;
    std::get<2>(*toReturn) = *third;

    return toReturn;
}


// TODO: since keys_3d_t is Pair<dcElement_t, dcElement_t>, there are no pointers, and nothing
// to free?? How is this right?
void freekeys_3d_t( const keys_3d_t* toFree )
{
    freeDCElem( &std::get<0>(*toFree) );
    freeDCElem( &std::get<1>(*toFree) );
    freeDCElem( &std::get<2>(*toFree) );
}


mapAccessTuple_3d_t* allocateMapAccessPair( size_t alphSize )
{
    mapAccessTuple_3d_t* toReturn;
    toReturn = (mapAccessTuple_3d_t*) malloc( sizeof(mapAccessTuple_3d_t) );
    std::get<0>(*toReturn) = *allockeys_3d_t(alphSize);
    std::get<1>(*toReturn) = *allocCostMedian_t(alphSize);
    return toReturn;
}


CostMatrix_3d::CostMatrix_3d( size_t alphSize, int* inTcm )
{
    alphabetSize = alphSize;
    size_t space = alphabetSize * alphabetSize * alphabetSize * sizeof(int);
    tcm = (int*) malloc(space);
    memcpy(tcm, inTcm, space);
    initializeMatrix();
}


CostMatrix_3d::~CostMatrix_3d()
{
    for ( auto& thing: myMatrix ) {
    // for ( mapIterator thing = myMatrix.begin(); thing != myMatrix.end(); thing++ ) {
        freeCostMedian_t(&std::get<1>(thing));

        // TODO: since keys_3d_t is tuple<dcElement_t, dcElement_t, dcElement_t>, there are no pointers, and nothing
        // to free?? How is this right? Anyway, skipping next line.
        // freekeys_3d_t(&thing.first);
    }
    myMatrix.clear();
    hasher.clear();

}


int CostMatrix_3d::getCostMedian( dcElement_t* first
                                , dcElement_t* second
                                , dcElement_t* third
                                , dcElement_t* retMedian
                                )
{
    keys_3d_t toLookup;
    std::get<0>(toLookup) = *first;
    std::get<1>(toLookup) = *second;
    std::get<2>(toLookup) = *third;
    mapIterator_3d found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        foundCost          = std::get<0>(std::get<1>(*found));
        retMedian->element = std::get<1>(std::get<1>(*found));
    }

    return foundCost;
}


int CostMatrix_3d::getSetCostMedian( dcElement_t* first
                                   , dcElement_t* second
                                   , dcElement_t* third
                                   , dcElement_t* retMedian
                                   )
{
    // not using allockeys_3d_t because we're making a copy of the packed characters _if we need to_
    keys_3d_t* toLookup = (keys_3d_t*) malloc( sizeof(keys_3d_t) );
    std::get<0>(*toLookup) = *first;
    std::get<1>(*toLookup) = *second;
    std::get<2>(*toLookup) = *third;
    mapIterator_3d found;
    int foundCost;

    if(DEBUG) {
        printf("1st: {%zu}: %" PRIu64 "\n", std::get<0>(*toLookup).alphSize, *std::get<0>(*toLookup).element ), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<1>(*toLookup).alphSize, *std::get<1>(*toLookup).element), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<2>(*toLookup).alphSize, *std::get<2>(*toLookup).element), fflush(stdout);
    }

    found = myMatrix.find(*toLookup);

    // if (retMedian->element == NULL) {
    //     retMedian->element = (packedChar*) calloc( dcElemSize(alphabetSize), sizeof(packedChar) );
    // }

    if ( found == myMatrix.end() ) {
        if(DEBUG)  printf( "\ngetSetCost didn't find %" PRIu64 " %" PRIu64 " %" PRIu64 ".\n"
                         , first->element[0], second->element[0], third->element[0] );

        costMedian_t* computedCostMed = computeCostMedian(*toLookup);
        //costMedian_t* computedCostMed = computeCostMedianFitchy(*toLookup);

        if(DEBUG) printf( "computed cost, median: %2i %" PRIu64 "\n"
                        , std::get<0>(*computedCostMed), std::get<1>(*computedCostMed)[0] );

        foundCost          = std::get<0>(*computedCostMed);
        retMedian->element = makePackedCharCopy( std::get<1>(*computedCostMed), alphabetSize, 1 );

        // Can't use allocateDCElement here, because makePackedCharCopy() allocates.
        // TODO: can I use tie() here? http://www.cplusplus.com/reference/tuple/tie/
        std::get<0>(*toLookup)          = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        std::get<0>(*toLookup).alphSize = first->alphSize;
        std::get<0>(*toLookup).element  = makePackedCharCopy( first->element , alphabetSize, 1 );

        std::get<1>(*toLookup)          = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        std::get<1>(*toLookup).alphSize = first->alphSize;
        std::get<1>(*toLookup).element  = makePackedCharCopy( second->element, alphabetSize, 1 );

        std::get<2>(*toLookup)          = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        std::get<2>(*toLookup).alphSize = first->alphSize;
        std::get<2>(*toLookup).element  = makePackedCharCopy( third->element, alphabetSize, 1 );

        setValue (toLookup, computedCostMed);
    } else {
        // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost          = std::get<0>(found->second);
        retMedian->element = makePackedCharCopy( std::get<1>(found->second), alphabetSize, 1 );
    }

    return foundCost;
}


costMedian_t* CostMatrix_3d::computeCostMedian(keys_3d_t keys)
{
    int curCost, minCost;

    size_t        elemArrLen = dcElemSize(alphabetSize);
//    packedChar*   median     = (packedChar*) calloc( elemArrLen, sizeof(uint64_t) );
    dcElement_t*  firstKey   = &std::get<0>(keys);
    dcElement_t*  secondKey  = &std::get<1>(keys);
    dcElement_t*  thirdKey   = &std::get<2>(keys);
    packedChar*   curMedian  = (packedChar*  ) calloc(elemArrLen, INT_WIDTH);  // don't free, it's going into toReturn
    costMedian_t* toReturn   = (costMedian_t*) malloc(sizeof(costMedian_t));   // array is alloc'ed above
    keys_t*       searchKey  = allockeys_t(alphabetSize);                      // This will be used for _2d_ lookup

    dcElement_t* singleNucleotide = &std::get<2>(*searchKey); // TODO: this is using third key. Check it.

    if(DEBUG) {
        for ( auto& thing: myMatrix ) {
            printf("%" PRIu64 " %" PRIu64 "\n", std::get<0>(thing.first).element[0], std::get<1>(thing.first).element[0]);
            printElemBits(&std::get<0>(thing.first));
            printElemBits(&std::get<1>(thing.first));
        }
    }

    curCost = minCost = INT_MAX;
    for (size_t curNucleotideIdx = 0; curNucleotideIdx < alphabetSize; ++curNucleotideIdx) {
        SetBit(singleNucleotide->element, curNucleotideIdx);

        // TODO: These need to be 2d lookups.
        curCost = CostMatrix::findDistance(searchKey, firstKey)
                + CostMatrix::findDistance(searchKey, secondKey)
                + CostMatrix::findDistance(searchKey, thirdKey);

        // now seemingly recreating logic in findDistance(). However, that was to get the cost for the
        // ambElem on each child; now we're combining those costs get overall cost and median
        if (curCost < minCost) {
            /*
            printf("\n--computeCostMedian: New low cost.\n");
            printf("    current nucleotide: %" PRIu64 " \n", *searchKey->second.element);
            printf("    found cost:      %d\n", curCost);
          */
            minCost = curCost;
            ClearAll(curMedian, elemArrLen);
            SetBit(curMedian, curNucleotideIdx);
        } else if (curCost == minCost) {
      /*
            printf("\nSame cost, new median.\n");
            printf("current nucleotide: %" PRIu64 " \n", *searchKey->second.element);
            printf("median: %" PRIu64 "\n", *curMedian);
            printf("found cost:      %d\n", curCost);
      */
            SetBit(curMedian, curNucleotideIdx);

            // printf("new median: %" PRIu64 "\n", *curMedian);
        }
        ClearBit(singleNucleotide->element, curNucleotideIdx);
    } // curNucleotideIdx
    std::get<0>(*toReturn) = minCost;
    std::get<1>(*toReturn) = curMedian;

    freekeys_3d_t(searchKey);
    free(searchKey);
    // freeDCElem(singleNucleotide);

    return toReturn;
}


/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
int CostMatrix_3d::findDistance (keys_3d_t* searchKey, dcElement_t* ambElem)
{
    mapIterator_3d found;
    int    minCost,
           curCost;
    size_t unambElemIdx;

    curCost = minCost = INT_MAX;

    for (size_t pos = 0; pos < alphabetSize; pos++) {
        if (TestBit( ambElem->element, pos )) {

            SetBit( std::get<0>(*searchKey).element, pos );
            found = myMatrix.find(*searchKey);

            if ( found == myMatrix.end() ) {
                // do unambiguous calculation here
                if( !isAmbiguous(ambElem->element, dcElemSize(alphabetSize)) ) {
                    unambElemIdx = 0;
                    while( unambElemIdx < alphabetSize && !TestBit(std::get<1>(*searchKey).element, unambElemIdx) ) {
                        unambElemIdx++;
                    }
                    curCost = tcm[pos * alphabetSize + unambElemIdx];
                    // printf("\n--findDistance-- \n    ambElemIdx: %zu, nucleotide: %zu, cost: %d\n", unambElemIdx, pos, curCost);
                } else {
                    printf("Something went wrong in the memoized cost matrix.\n");
                    printf("missing key: %" PRIu64 " %" PRIu64 "\n", *std::get<0>(*searchKey).element, *std::get<1>(*searchKey).element);
                    exit(1);
                }
            } else {
        // We found the memoized cost for the elements in the TCM.
                curCost = std::get<0>(found->second);
            }
            if (curCost < minCost) { minCost = curCost; }

            ClearBit( std::get<0>(*searchKey).element, pos );
        }
    }

    if (DEBUG) {
        printf( "distance ambElem: %" PRIu64 ", nucleotide: %" PRIu64 "\n"
              , ambElem->element[0]
              , *std::get<1>(*searchKey).element
              );

        printf( "cost: %i\n", minCost );
    }

    return minCost;
}


void CostMatrix_3d::initializeMatrix ()
{
   // keys_3d_t* keys;
   // costMedian_t* costMedian;
   // mapAccessTuple_3d_t* toInsert;

    dcElement_t* firstKey  = allocateDCElement( alphabetSize );
    dcElement_t* secondKey = allocateDCElement( alphabetSize );
    dcElement_t* thirdKey  = allocateDCElement( alphabetSize );
    dcElement_t* retMedian = allocateDCElement( alphabetSize );
    // packedChar*  median;
//    int cost;

    for (size_t key1 = 0; key1 < alphabetSize; key1++) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);

        // key2 starts from 0, so non-symmetric matrices should work
        for (size_t key2 = 0; key2 < alphabetSize; key2++) { // doesn't assumes 0 diagonal
            // First three have been alloc'ed in allocate fn. above, so use only pointers.
            // median is allocated as a pointer, so don't need to derefernce.
            // firstKey  = &toInsert->first.first;
            // secondKey = &toInsert->first.second;
            // cost      = &toInsert->second.first;
            // median    =  toInsert->second.second;

            // We allocated a new pair above, so we never will clear the bits set here.
            // SetBit(firstKey->element, key1);
            // SetBit(median, key1);

            SetBit(secondKey->element, key2);
            // SetBit(median, key2);

            CostMatrix_3d::getSetCostMedian(firstKey, secondKey, thirdKey, retMedian);
            // cost = CostMatrix::getSetCostMedian(firstKey, secondKey, retMedian);

            // if(DEBUG) {
            //     printf("keys set: %" PRIu64 " %" PRIu64 "\n", *firstKey->element, *secondKey->element);
            //     printf("median: %" PRIu64 "   cost: %i\n", *toInsert->second.second, toInsert->second.first);
            // }

           // myMatrix.insert(*toInsert);
             ClearBit(secondKey->element, key2);

        } // key2
        ClearBit(firstKey->element, key1);
    }
    // printf("finished initializing\n");
    // printf("freed keys\n");
}


void CostMatrix_3d::setValue(keys_3d_t* key, costMedian_t* median)
{
    myMatrix.insert(std::make_pair(*key, *median));
}

/******* !!DEPRECATED!! *******/
/*
costMedian_t* CostMatrix::computeCostMedianFitchy(keys_3d_t keys) {

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
*/
