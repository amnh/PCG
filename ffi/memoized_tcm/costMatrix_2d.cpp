#include <cstring> //for memcpy;
#include <inttypes.h>

#include "costMatrix_2d.h"
#include "dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

constexpr unsigned int CostMatrix_2d::defaultExtraGapCostMetric[25];
constexpr unsigned int CostMatrix_2d::defaultDiscreteMetric[25];
constexpr unsigned int CostMatrix_2d::defaultL1NormMetric[25];


costMatrix_p construct_CostMatrix_2d_C( size_t alphSize, unsigned int* tcm )
{
    return new CostMatrix_2d( alphSize, tcm );
}


void destruct_CostMatrix_2d_C( costMatrix_p untyped_self )
{
    delete static_cast<CostMatrix_2d*> (untyped_self);
}


unsigned int call_getSetCost_2d_C( costMatrix_p untyped_self
                                 , dcElement_t* first
                                 , dcElement_t* second
                                 , dcElement_t* retMedian
                                 )
{
    CostMatrix_2d* thisMtx = static_cast<CostMatrix_2d*> (untyped_self);
    return thisMtx->getSetCostMedian(first, second, retMedian);
}


void freeCostMedian_t ( costMedian_t* toFree )
{
    if (std::get<1>(*toFree) == NULL)
      return;

    std::free( std::get<1>(*toFree) );
    std::get<1>(*toFree) = NULL;
}


keys_2d_t* allocKeys_2d_t( size_t alphabetSize )
{
    auto toReturn = new keys_2d_t;

    // jump through these hoops because I'm dereferencing the two elements,
    // and I couldn't free the pointers otherwise.
    auto firstElement  = allocateDCElement(alphabetSize);
    auto secondElement = allocateDCElement(alphabetSize);

    std::get<0>(*toReturn) = *firstElement;
    std::get<1>(*toReturn) = *secondElement;

    std::free( firstElement );
    std::free( secondElement );

    std::get<0>(*toReturn).alphSize = std::get<1>(*toReturn).alphSize
                                    = alphabetSize;

    return toReturn;
}


void freeKeys_2d_t( const keys_2d_t* toFree )
{
    freeDCElem( &std::get<0>(*toFree) );
    freeDCElem( &std::get<1>(*toFree) );
}


CostMatrix_2d::CostMatrix_2d()
  : alphabetSize(5)
  , elementSize(1)
{
    initializeTCM(defaultExtraGapCostMetric);
    initializeMatrix();
}


CostMatrix_2d::CostMatrix_2d( size_t alphSize, unsigned int* inTcm )
  : alphabetSize(alphSize)
  , elementSize(dcElemSize(alphSize))
{
    initializeTCM(inTcm);
    initializeMatrix();
}


CostMatrix_2d::~CostMatrix_2d()
{
    for ( auto iterator = myMatrix.begin(); iterator != myMatrix.end(); iterator++ ) {
        freeCostMedian_t( &std::get<1>(*iterator) );
        freeKeys_2d_t( &std::get<0>(*iterator) );
    }
    std::free(tcm);
    myMatrix.clear();
}


unsigned int CostMatrix_2d::getCostMedian( dcElement_t* first
                                         , dcElement_t* second
                                         , dcElement_t* retMedian
                                         )
{
    const auto toLookup = std::make_tuple(*first, *second);
    auto foundCost{0};

    const auto found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        const auto foundValue = std::get<1>(*found);
        if (retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(foundValue), alphabetSize, 1 );
        foundCost          = std::get<0>(foundValue);
    }

    // don't need to free toLookup because it only contains pointers to incoming dc_Elements which
    // will be dealloc'ed elsewhere
    return foundCost;
}


unsigned int CostMatrix_2d::getSetCostMedian( dcElement_t* first
                                            , dcElement_t* second
                                            , dcElement_t* retMedian
                                            )
{
    const auto toLookup = std::make_tuple(*first, *second);
    const auto found    = myMatrix.find(toLookup);
    auto foundCost{0};

    if(DEBUG) {
        printf("1st: {%zu}: %" PRIu64 "\n", std::get<0>(toLookup).alphSize, *std::get<0>(toLookup).element ), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<1>(toLookup).alphSize, *std::get<1>(toLookup).element), fflush(stdout);
    }

    if ( found == myMatrix.end() ) {
        if(DEBUG) printf( "\ngetSetCost didn't find %" PRIu64 " %" PRIu64 ".\n"
                        , first->element[0], second->element[0] );

        const auto computedCostMed = computeCostMedian(toLookup);
        //costMedian_t* computedCostMed = computeCostMedianFitchy(*toLookup);

        if(DEBUG) printf( "computed cost, median: %2i %" PRIu64 "\n"
                        , std::get<0>(*computedCostMed), std::get<1>(*computedCostMed)[0] );

        foundCost = std::get<0>(*computedCostMed);

        if(retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(*computedCostMed), alphabetSize, 1 );

        setValue(first, second, computedCostMed);
        freeCostMedian_t(computedCostMed);
        delete computedCostMed;
        // freeMapAccessTuple_t(toLookup);
    } else {
        // because in the next two lines, I get back a tuple<keys_2d_t, costMedian_t>
        foundCost = std::get<0>(std::get<1>(*found));
        if(retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(std::get<1>(*found)), alphabetSize, 1 );
    }
    // don't need to free toLookup because it only contains pointers to input dc_Elements which will be dealloc'ed elsewhere
    if(DEBUG) printf("Matrix Value Count: %lu\n", myMatrix.size());

    return foundCost;
}


costMedian_t* CostMatrix_2d::computeCostMedian(keys_2d_t keys)
{
    auto curCost{INT_MAX},
         minCost{INT_MAX};

    auto toReturn  = new costMedian_t;   // array is alloc'ed above
    auto curMedian = allocatePackedChar(alphabetSize, 1);   // don't free, it's going into toReturn

    const auto firstKey  = &std::get<0>(keys);
    const auto secondKey = &std::get<1>(keys);

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

    for (size_t symbolIndex = 0; symbolIndex < alphabetSize; ++symbolIndex) {
        curCost = findDistance(symbolIndex, firstKey)
                + findDistance(symbolIndex, secondKey);

        if (DEBUG) {
            printf("Before Minimization Logic:\n");
            printf("  Symbol Index: %" PRIu64 " \n", symbolIndex);
            printf("  Current Cost: %d\n", curCost);
            printf("  Minimal Cost: %d\n", minCost);
            printPackedChar( curMedian, 1, alphabetSize);
            printf("\n\n");
        }
        
        // now seemingly recreating logic in findDistance(). However, that was to get the cost for the
        // ambElem on each child; now we're combining those costs get overall cost and median
        if (curCost < minCost) {
            /*
            printf("\n--computeCostMedian: New low cost.\n");
            printf("    current nucleotide: %" PRIu64 " \n", *std::get<1>(searchKey).element);
            printf("    found cost:      %d\n", curCost);
          */
            minCost = curCost;
            ClearAll(curMedian, elementSize);
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

        if (DEBUG) {
          printf("After Minimization Logic:\n");
          printf("  Minimal Cost: %d\n", minCost);
          printPackedChar( curMedian, 1, alphabetSize);
          printf("\n\n");
          fflush(stdout);
        }
        
    }
    std::get<0>(*toReturn) = minCost;
    std::get<1>(*toReturn) = curMedian;

    return toReturn;
}


/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
unsigned int CostMatrix_2d::findDistance (size_t fixedSymbolIndex, dcElement_t* ambElem)
{
    auto minCost{INT_MAX},
         curCost{INT_MAX};

    for (size_t ambiguitySymbolIndex = 0; ambiguitySymbolIndex < alphabetSize; ++ambiguitySymbolIndex) {
        if (TestBit( ambElem->element, ambiguitySymbolIndex )) {
            curCost = tcm[fixedSymbolIndex * alphabetSize + ambiguitySymbolIndex];
            if ( curCost < minCost ) {
                minCost = curCost;
            }
        }
    }

    return minCost;
}


void CostMatrix_2d::initializeMatrix()
{
    const auto firstKey  = allocateDCElement( alphabetSize );
    const auto secondKey = allocateDCElement( alphabetSize );
    const auto toLookup  = std::make_tuple(*firstKey, *secondKey);

    for (size_t firstKey_bit = 0; firstKey_bit < alphabetSize; ++firstKey_bit) { // for every possible value of firstKey_bit, secondKey_bit
        SetBit(firstKey->element, firstKey_bit);

        // secondKey_bit starts from 0, so non-symmetric matrices should work
        for (size_t secondKey_bit = 0; secondKey_bit < alphabetSize; ++secondKey_bit) { // doesn't assumes 0 diagonal
            if (DEBUG) printf("Insert key1_bit: %3zu, key2_bit: %3zu\n", firstKey_bit, secondKey_bit);
            SetBit(secondKey->element, secondKey_bit);

            auto toInsert = computeCostMedian(toLookup);
            
            // Originally used `getSetCostMedian()` here, but it involves a lot of overhead and we know we're only
            // using unambiguous elems, so we can just insert.
            //std::get<0>(*toInsert) = tcm[firstKey_bit * alphabetSize + secondKey_bit];
            // TODO: can I move the allocation out of `packedCharOr()`?
            //if (std::get<1>(*toInsert) != NULL) {
            //    std::free( std::get<1>(*toInsert) );
            //}
            //std::get<1>(*toInsert) = packedCharOr(firstKey->element, secondKey->element, alphabetSize, 1);
            

            setValue(firstKey, secondKey, toInsert);
            freeCostMedian_t(toInsert);
            delete toInsert;
            ClearBit(secondKey->element, secondKey_bit);
        } // secondKey_bit
        ClearBit(firstKey->element, firstKey_bit);
        // ClearBit(std::get<1>(*toInsert), firstKey_bit);
    }
    // Just to reiterate, getSetCostMedian() should allocate, so we should dealloc these.
    freeDCElem(firstKey);        // deallocate array
    freeDCElem(secondKey);
    std::free(firstKey);         // free pointer
    std::free(secondKey);
    // printf("finished initializing\n");
    // printf("freed keys\n");
}


void CostMatrix_2d::initializeTCM(const unsigned int* const inputBuffer)
{
    const auto bufferSize = alphabetSize * alphabetSize * sizeof(*tcm);
    tcm = (unsigned int*) std::malloc( bufferSize );
    std::memcpy( tcm, inputBuffer, bufferSize );
}


void CostMatrix_2d::setValue( const dcElement_t*  const first
                            , const dcElement_t*  const second
                            , const costMedian_t* const toInsert
                            )
{
    // Making a deep copy of key & median here to help with memory management in calling fns.

    // Create a deep copy of the toInsert value to insert.
    const auto value = std::make_tuple( std::get<0>(*toInsert)
                                      , makePackedCharCopy( std::get<1>(*toInsert), alphabetSize, 1 )
                                      );

    // Create a new 2-tuple key to insert.
    const auto key = new keys_2d_t;

    // Copy the first element into key.
    const auto firstElem       = new dcElement_t;
    std::get<0>(*key)          = *firstElem;
    std::get<0>(*key).alphSize = alphabetSize;
    std::get<0>(*key).element  = makePackedCharCopy(first->element, alphabetSize, 1);

    // Copy the second element into key.
    const auto secondElem      = new dcElement_t;
    std::get<1>(*key)          = *secondElem;
    std::get<1>(*key).alphSize = alphabetSize;
    std::get<1>(*key).element  = makePackedCharCopy(second->element, alphabetSize, 1);

    // Add the copied key-value pair to the matrix.
    // This has to be a pair!
    // Clang is okay with make_tuple() or forward_as_tuple(), but gcc doesn't like it.
    myMatrix.insert(std::make_pair(*key, value));
    delete key;
    delete firstElem;
    delete secondElem;
}
