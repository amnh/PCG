#include <cstring> //for memcpy;
#include <inttypes.h>

#include "costMatrix_2d.h"
#include "costMatrix_3d.h"
#include "dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

costMatrix_p construct_CostMatrix_3d_C( size_t alphSize, unsigned int* tcm )
{
    return new CostMatrix_3d( alphSize, tcm );
}


void destruct_CostMatrix_3d_C( costMatrix_p untyped_self )
{
    delete static_cast<CostMatrix_3d*> (untyped_self);
}


unsigned int call_getSetCost_3d_C( costMatrix_p untyped_self
                                 , dcElement_t* first
                                 , dcElement_t* second
                                 , dcElement_t* third
                                 , dcElement_t* retMedian
                                 )
{
    CostMatrix_3d* thisMtx = static_cast<CostMatrix_3d*> (untyped_self);
    return thisMtx->getSetCostMedian(first, second, third, retMedian);
}


keys_3d_t* allocKeys_3d_t( size_t alphabetSize )
{
    auto toReturn = new keys_3d_t;

    // jump through these hoops because I'm dereferencing the two elements,
    // and I couldn't free the pointers otherwise.
    auto firstElement  = allocateDCElement(alphabetSize);
    auto secondElement = allocateDCElement(alphabetSize);
    auto thirdElement  = allocateDCElement(alphabetSize);

    std::get<0>(*toReturn) = *firstElement;
    std::get<1>(*toReturn) = *secondElement;
    std::get<2>(*toReturn) = *thirdElement;

    std::free( firstElement );
    std::free( secondElement );
    std::free( thirdElement );

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


CostMatrix_3d::CostMatrix_3d()
{
    twoD_matrix = new CostMatrix_2d();
}


CostMatrix_3d::CostMatrix_3d( size_t alphSize, unsigned int* inTcm )
{
    twoD_matrix = new CostMatrix_2d(alphSize, inTcm);
}


CostMatrix_3d::~CostMatrix_3d()
{
    for ( auto iterator = myMatrix.begin(); iterator != myMatrix.end(); iterator++ ) {
        freeCostMedian_t( &std::get<1>(*iterator) );
        freeKeys_3d_t( &std::get<0>(*iterator) );
    }
    delete twoD_matrix;
    myMatrix.clear();
}


unsigned int CostMatrix_3d::getCostMedian( dcElement_t* first
                                         , dcElement_t* second
                                         , dcElement_t* third
                                         , dcElement_t* retMedian
                                         )
{
    const auto toLookup = std::make_tuple(*first, *second, *third);
    auto foundCost{0};

    const auto found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        const auto foundValue = std::get<1>(*found);
        if (retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(foundValue), twoD_matrix->alphabetSize, 1 );
        foundCost          = std::get<0>(foundValue);
     }

    // don't need to free toLookup because it only contains pointers to incoming dc_Elements which
    // will be dealloc'ed elsewhere
    return foundCost;
}


unsigned int CostMatrix_3d::getSetCostMedian( dcElement_t* first
                                            , dcElement_t* second
                                            , dcElement_t* third
                                            , dcElement_t* retMedian
                                            )
{
    const auto toLookup = std::make_tuple(*first, *second, *third);
    const auto found    = myMatrix.find(toLookup);
    auto foundCost{0};

    if(DEBUG) {
        printf("1st: {%zu}: %" PRIu64 "\n", std::get<0>(toLookup).alphSize, *std::get<0>(toLookup).element), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<1>(toLookup).alphSize, *std::get<1>(toLookup).element), fflush(stdout);
        printf("3rd: {%zu}: %" PRIu64 "\n", std::get<2>(toLookup).alphSize, *std::get<2>(toLookup).element), fflush(stdout);
    }

    if ( found == myMatrix.end() ) {
        if(DEBUG) printf( "\ngetSetCost didn't find %" PRIu64 " %" PRIu64 " %" PRIu64 ".\n"
                         , first->element[0], second->element[0], third->element[0] );

        const auto computedCostMed = computeCostMedian(toLookup);
        //costMedian_t* computedCostMed = computeCostMedianFitchy(*toLookup);

        if(DEBUG) printf( "computed cost, median: %2i %" PRIu64 "\n"
                        , std::get<0>(*computedCostMed), std::get<1>(*computedCostMed)[0] );

        foundCost          = std::get<0>(*computedCostMed);

        if(retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(*computedCostMed), twoD_matrix->alphabetSize, 1 );

        setValue(first, second, third, computedCostMed);
        freeCostMedian_t(computedCostMed);
        delete computedCostMed;
        // freeMapAccessTuple_t(toLookup);
    } else {
        // because in the next two lines, I get back a tuple<keys_3d_t, costMedian_t>
        foundCost = std::get<0>(std::get<1>(*found));
        if(retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(std::get<1>(*found)), twoD_matrix->alphabetSize, 1 );
    }

    // don't need to free toLookup because it only contains pointers to input dc_Elements which will be dealloc'ed elsewhere
    if(DEBUG) printf("Matrix Value Count: %lu\n", myMatrix.size());

    return foundCost;
}


costMedian_t* CostMatrix_3d::computeCostMedian(keys_3d_t keys)
{
    auto curCost{UINT_MAX},
         minCost{UINT_MAX};

    const auto symbolCount = twoD_matrix->alphabetSize; // For efficiency, dereference less.
    const auto firstKey    = &std::get<0>(keys);
    const auto secondKey   = &std::get<1>(keys);
    const auto thirdKey    = &std::get<2>(keys);
    const auto curMedian   = allocatePackedChar(symbolCount, 1);   // don't free, it's going into toReturn

    if(DEBUG) {
        for ( auto& thing: myMatrix ) {
            printf( "%" PRIu64 " %" PRIu64 "\n"
                  , std::get<0>(std::get<0>(thing)).element[0]
                  , std::get<1>(std::get<0>(thing)).element[0]
                  );
            printElemBits( &std::get<0>(std::get<0>(thing)) );
            printElemBits( &std::get<1>(std::get<0>(thing)) );
        }
    }

    for (size_t symbolIndex = 0; symbolIndex < symbolCount; ++symbolIndex) {

        curCost = twoD_matrix->findDistance(symbolIndex, firstKey)
                + twoD_matrix->findDistance(symbolIndex, secondKey)
                + twoD_matrix->findDistance(symbolIndex, thirdKey);

        if (curCost < minCost) {
            /*
            printf("\n--computeCostMedian: New low cost.\n");
            printf("    current nucleotide: %" PRIu64 " \n", *std::get<1>(searchKey).element);
            printf("    found cost:      %d\n", curCost);
          */
            minCost = curCost;
            ClearAll(curMedian, twoD_matrix->elementSize);
            SetBit(curMedian, symbolIndex);
        } else if (curCost == minCost) {
      /*
            printf("\nSame cost, new median.\n");
            printf("current nucleotide: %" PRIu64 " \n", *std::get<1>(searchKey).element);
            printf("median: %" PRIu64 "\n", *curMedian);
            printf("found cost:      %d\n", curCost);
      */
            SetBit(curMedian, symbolIndex);

            // printf("new median: %" PRIu64 "\n", *curMedian);
        }
    }

    const auto toReturn = new costMedian_t;
    std::get<0>(*toReturn) = minCost;
    std::get<1>(*toReturn) = curMedian;

    return toReturn;
}


void CostMatrix_3d::setValue( const dcElement_t*  const first
                            , const dcElement_t*  const second
                            , const dcElement_t*  const third
                            , const costMedian_t* const toInsert
                            )
{
    // For efficiency, dereference less.
    const auto symbolCount = twoD_matrix->alphabetSize;

    // Making a deep copy of key & median here to help with memory management in calling fns.

    // Create a deep copy of the toInsert value to insert.
    const auto value = std::make_tuple( std::get<0>(*toInsert)
                                      , makePackedCharCopy( std::get<1>(*toInsert), symbolCount, 1 )
                                      );

    // Create a new 3-tuple key to insert.
    const auto key = new keys_3d_t;

    // Copy the first element into key.
    const auto firstElem       = new dcElement_t;
    std::get<0>(*key)          = *firstElem;
    std::get<0>(*key).alphSize = twoD_matrix->alphabetSize;
    std::get<0>(*key).element  = makePackedCharCopy(first->element, symbolCount, 1);

    // Copy the second element into key.
    const auto secondElem      = new dcElement_t;
    std::get<1>(*key)          = *secondElem;
    std::get<1>(*key).alphSize = twoD_matrix->alphabetSize;
    std::get<1>(*key).element  = makePackedCharCopy(second->element, symbolCount, 1);

    // Copy the third element into key.
    const auto thirdElem       = new dcElement_t;
    std::get<2>(*key)          = *thirdElem;
    std::get<2>(*key).alphSize = twoD_matrix->alphabetSize;
    std::get<2>(*key).element  = makePackedCharCopy(third->element, symbolCount, 1);

    // Add the copied key-value pair to the matrix.
    // This has to be a pair!
    // Clang is okay with make_tuple() or forward_as_tuple(), but gcc doesn't like it.
    myMatrix.insert(std::make_pair(*key, value));
    delete key;
    delete firstElem;
    delete secondElem;
    delete thirdElem;
}
