#include <cstring> //for memcpy;
#include <inttypes.h>

#include "costMatrix.h"
#include "costMatrix_3d.h"
#include "dynamicCharacterOperations.h"

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
int call_getSetCost_3d_C( costMatrix_p untyped_self
                        , dcElement_t* first
                        , dcElement_t* second
                        , dcElement_t* third
                        , dcElement_t* retMedian
                        )
{
    CostMatrix_3d* thisMtx = static_cast<CostMatrix_3d*> (untyped_self);
    return thisMtx->getSetCostMedian(first, second, third, retMedian);
}


keys_3d_t* allockeys_3d_t( size_t alphabetSize )
{
    auto toReturn = new keys_3d_t;

    std::get<0>(*toReturn) = *allocateDCElement(alphabetSize);
    std::get<1>(*toReturn) = *allocateDCElement(alphabetSize);
    std::get<2>(*toReturn) = *allocateDCElement(alphabetSize);

    std::get<0>(*toReturn).alphSize = std::get<1>(*toReturn).alphSize
                                    = std::get<2>(*toReturn).alphSize
                                    = alphabetSize;

    return toReturn;
}


void freeKeys_3d_t( const keys_3d_t* toFree )
{
    freeDCElem( &std::get<0>(*toFree) );
    freeDCElem( &std::get<1>(*toFree) );
    freeDCElem( &std::get<2>(*toFree) );
}


CostMatrix_3d::CostMatrix_3d( size_t alphSize, int* inTcm )
{
    alphabetSize = alphSize;
    tcm = new int[alphabetSize * alphabetSize * alphabetSize];
    memcpy(tcm, inTcm, alphabetSize * alphabetSize * alphabetSize);
    initializeMatrix(); // should only have to do this for 3d, as 2d is initialized by its own constructor
    twoD_matrix = CostMatrix(alphSize, inTcm);
}


CostMatrix_3d::~CostMatrix_3d()
{
    for ( auto& thing: myMatrix ) {
        freeKeys_3d_t(&std::get<0>(thing));
        // freeCostMedian_t(&std::get<1>(thing));
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
    auto toLookup = new keys_3d_t;
    std::get<0>(*toLookup) = *first;
    std::get<1>(*toLookup) = *second;
    std::get<2>(*toLookup) = *third;
    auto foundCost{0};

    auto found = myMatrix.find(*toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        foundCost          = std::get<0>(std::get<1>(*found));
        retMedian->element = std::get<1>(std::get<1>(*found));
    }

    // don't need to free toLookup because it only contains pointers to incoming dc_Elements which will be dealloc'ed elsewhere
    return foundCost;
}


int CostMatrix_3d::getSetCostMedian( dcElement_t* first
                                   , dcElement_t* second
                                   , dcElement_t* third
                                   , dcElement_t* retMedian
                                   )
{
    // not using allockeys_3d_t because we're making a copy of the packed characters _if we need to_
    auto toLookup = new keys_3d_t;
    std::get<0>(*toLookup) = *first;
    std::get<1>(*toLookup) = *second;
    std::get<2>(*toLookup) = *third;
    auto found = myMatrix.find(*toLookup);
    auto foundCost{0};

    if(DEBUG) {
        printf("1st: {%zu}: %" PRIu64 "\n", std::get<0>(*toLookup).alphSize, *std::get<0>(*toLookup).element ), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<1>(*toLookup).alphSize, *std::get<1>(*toLookup).element), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<2>(*toLookup).alphSize, *std::get<2>(*toLookup).element), fflush(stdout);
    }

    // if (retMedian->element == NULL) {
    //     retMedian->element = (packedChar*) calloc( dcElemSize(alphabetSize), sizeof(packedChar) );
    // }

    if ( found == myMatrix.end() ) {
        if(DEBUG)  printf( "\ngetSetCost didn't find %" PRIu64 " %" PRIu64 " %" PRIu64 ".\n"
                         , first->element[0], second->element[0], third->element[0] );

        auto computedCostMed = computeCostMedian(*toLookup);
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
        std::get<1>(*toLookup).alphSize = second->alphSize;
        std::get<1>(*toLookup).element  = makePackedCharCopy( second->element, alphabetSize, 1 );

        std::get<2>(*toLookup)          = *(dcElement_t*) malloc( sizeof(dcElement_t) );
        std::get<2>(*toLookup).alphSize = third->alphSize;
        std::get<2>(*toLookup).element  = makePackedCharCopy( third->element, alphabetSize, 1 );

        setValue (toLookup, computedCostMed);
    } else {
        // because in the next two lines, I get back a tuple<keys_t, costMedian_t>
        foundCost          = std::get<0>(found->second);
        retMedian->element = makePackedCharCopy( std::get<1>(found->second), alphabetSize, 1 );
    }

    // don't need to free toLookup because it only contains pointers to input dc_Elements which will be dealloc'ed elsewhere
    return foundCost;
}


costMedian_t* CostMatrix_3d::computeCostMedian(keys_3d_t keys)
{
    auto curCost{INT_MAX},
         minCost{INT_MAX};

    auto elemArrLen = dcElemSize(alphabetSize);
//    packedChar*   median     = (packedChar*) calloc( elemArrLen, sizeof(uint64_t) );
    // TODO: can I use `auto` in the next three lines?
    auto firstKey  = &std::get<0>(keys);
    auto secondKey = &std::get<1>(keys);
    auto thirdKey  = &std::get<2>(keys);
    auto toReturn  = new costMedian_t;   // array is alloc'ed above

    packedChar* curMedian = (packedChar*) calloc(elemArrLen, INT_WIDTH);             // don't free, it's going into toReturn
    auto searchKey = allocKeys_t(alphabetSize);  // This will be used for _2d_ lookup
    auto singleNucleotide = &std::get<1>(*searchKey); // TODO: this is using third key. Check it.

    if(DEBUG) {
        for ( auto& thing: myMatrix ) {
            printf("%" PRIu64 " %" PRIu64 "\n"
                  , std::get<0>(std::get<0>(thing)).element[0]
                  , std::get<1>(std::get<0>(thing)).element[0]
                  );

            printElemBits(&std::get<0>(thing.first));
            printElemBits(&std::get<1>(thing.first));
        }
    }

    for (size_t curNucleotideIdx = 0; curNucleotideIdx < alphabetSize; ++curNucleotideIdx) {
        SetBit(singleNucleotide->element, curNucleotideIdx);

        curCost = twoD_matrix.findDistance(searchKey, firstKey)
                + twoD_matrix.findDistance(searchKey, secondKey)
                + twoD_matrix.findDistance(searchKey, thirdKey);

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

    freeKeys_t(searchKey);

    return toReturn;
}


/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
int CostMatrix_3d::findDistance (keys_3d_t* searchKey, dcElement_t* ambElem)
{
    mapIterator_3d found;
    auto minCost{INT_MAX},
         curCost{INT_MAX};
    size_t unambElemIdx;

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
    auto firstKey  = allocateDCElement( alphabetSize );
    auto secondKey = allocateDCElement( alphabetSize );
    auto thirdKey  = allocateDCElement( alphabetSize );
    auto retMedian = allocateDCElement( alphabetSize );

    for (size_t key1 = 0; key1 < alphabetSize; key1++) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);

        // key2 and key3 start from 0, so non-symmetric matrices should work
        for (size_t key2 = 0; key2 < alphabetSize; key2++) { // doesn't assumes 0 diagonal
            SetBit(secondKey->element, key2);

            for (size_t key3 = 0; key3 < alphabetSize; key3++) {
                SetBit(thirdKey->element, key3);
                CostMatrix_3d::getSetCostMedian(firstKey, secondKey, thirdKey, retMedian);

                ClearBit(thirdKey->element, key3);
            } // key3
            ClearBit(secondKey->element, key2);
        } // key2
        ClearBit(firstKey->element, key1);
    }
    // printf("finished initializing\n");
    // TODO: do I need to free keys?
    freeDCElem(firstKey);
    freeDCElem(secondKey);
    freeDCElem(thirdKey);
    freeDCElem(retMedian);
    // printf("freed keys\n");

}


void CostMatrix_3d::setValue(keys_3d_t* key, costMedian_t* median)
{
    // This has to be a pair. Clang is okay with make_tuple() or forward_as_tuple(), but gcc doesn't like it.
    myMatrix.insert(std::make_pair(*key, *median));
}
