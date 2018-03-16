#include <cstring> //for memcpy;
#include <inttypes.h>

#include "costMatrix_2d.h"
#include "costMatrix_3d.h"
#include "dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

constexpr unsigned int CostMatrix_3d::defaultExtraGapCostMetric[25];
constexpr unsigned int CostMatrix_3d::defaultDiscreteMetric[25];
constexpr unsigned int CostMatrix_3d::defaultL1NormMetric[25];


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
  : alphabetSize(5)
  , elementSize(1)
{
    // should only have to do this for 3d, as 2d is initialized by its own constructor
    initializeTCM(defaultExtraGapCostMetric);
    // Don't need to initialize matrix because we're always going to look up in 2d matrix first
    // initializeMatrix();inTcm
    twoD_matrix = new CostMatrix_2d(alphabetSize, tcm);
}


CostMatrix_3d::CostMatrix_3d( size_t alphSize, unsigned int* inTcm )
  : alphabetSize(alphSize)
  , elementSize(dcElemSize(alphSize))
{
    initializeTCM(inTcm);
    // Don't need to initialize matrix because we're always going to look up in 2d matrix first
    // initializeMatrix();
    // should only have to do this for 3d, as 2d is initialized by its own constructor
    twoD_matrix = new CostMatrix_2d(alphSize, inTcm);
}


CostMatrix_3d::~CostMatrix_3d()
{
    for ( auto iterator = myMatrix.begin(); iterator != myMatrix.end(); iterator++ ) {
        freeCostMedian_t( &std::get<1>(*iterator) );
        freeKeys_3d_t( &std::get<0>(*iterator) );
    }
    // twoD_matrix->~CostMatrix_2d();
    delete twoD_matrix;
    std::free(tcm);
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
        retMedian->element = makePackedCharCopy( std::get<1>(foundValue), alphabetSize, 1 );
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
        retMedian->element = makePackedCharCopy( std::get<1>(*computedCostMed), alphabetSize, 1 );

        setValue(first, second, third, computedCostMed);
        freeCostMedian_t(computedCostMed);
        delete computedCostMed;
        // freeMapAccessTuple_t(toLookup);
    } else {
        // because in the next two lines, I get back a tuple<keys_3d_t, costMedian_t>
        foundCost = std::get<0>(std::get<1>(*found));
        if(retMedian->element != NULL) std::free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(std::get<1>(*found)), alphabetSize, 1 );
    }

    // don't need to free toLookup because it only contains pointers to input dc_Elements which will be dealloc'ed elsewhere
    if(DEBUG) printf("Matrix Value Count: %lu\n", myMatrix.size());

    return foundCost;
}


costMedian_t* CostMatrix_3d::computeCostMedian(keys_3d_t keys)
{
    auto curCost{INT_MAX},
         minCost{INT_MAX};

    auto elemArrLen = dcElemSize(alphabetSize);
//    packedChar*   median     = (packedChar*) calloc( elemArrLen, sizeof(uint64_t) );
    auto firstKey  = &std::get<0>(keys);
    auto secondKey = &std::get<1>(keys);
    auto thirdKey  = &std::get<2>(keys);
    auto toReturn  = new costMedian_t;   // array is alloc'ed above
    auto curMedian = allocatePackedChar(alphabetSize, 1);   // don't free, it's going into toReturn

    auto searchKey        = allocKeys_2d_t(alphabetSize);
    auto singleNucleotide = &std::get<1>(*searchKey); // TODO: this is using third key. Check it.

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

    for (size_t curNucleotideIdx = 0; curNucleotideIdx < alphabetSize; ++curNucleotideIdx) {
        SetBit(singleNucleotide->element, curNucleotideIdx);

        curCost = twoD_matrix->findDistance(searchKey, firstKey)
                + twoD_matrix->findDistance(searchKey, secondKey)
                + twoD_matrix->findDistance(searchKey, thirdKey);

        // now seemingly recreating logic in findDistance(). However, that was to get the cost for the
        // ambElem on each child; now we're combining those costs get overall cost and median
        if (curCost < minCost) {
            /*
            printf("\n--computeCostMedian: New low cost.\n");
            printf("    current nucleotide: %" PRIu64 " \n", *std::get<1>(searchKey).element);
            printf("    found cost:      %d\n", curCost);
          */
            minCost = curCost;
            ClearAll(curMedian, elemArrLen);
            SetBit(curMedian, curNucleotideIdx);
        } else if (curCost == minCost) {
      /*
            printf("\nSame cost, new median.\n");
            printf("current nucleotide: %" PRIu64 " \n", *std::get<1>(searchKey).element);
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

    freeKeys_2d_t(searchKey);
    delete searchKey;

    return toReturn;
}


/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
unsigned int CostMatrix_3d::findDistance (keys_3d_t* searchKey, dcElement_t* ambElem)
{
    auto minCost{INT_MAX},
         curCost{INT_MAX};
    size_t unambElemIdx;

    for (size_t pos = 0; pos < alphabetSize; ++pos) {
        if (TestBit( ambElem->element, pos )) {

            SetBit( std::get<0>(*searchKey).element, pos );
            const auto found = myMatrix.find(*searchKey);

            if (found == myMatrix.end()) {
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
            } else {  // We found the memoized cost for the elements in the TCM.
                curCost = std::get<0>(std::get<1>(*found));
            }
            if (curCost < minCost) {
                minCost = curCost;
            }
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


// TODO: Make sure this logic is right. Do I need to do something Sankoffian here?
void CostMatrix_3d::initializeMatrix()
{
    const auto firstKey  = allocateDCElement( alphabetSize );
    const auto secondKey = allocateDCElement( alphabetSize );
    const auto thirdKey  = allocateDCElement( alphabetSize );
    const auto toInsert  = new costMedian_t;

    for (size_t firstKey_bit = 0; firstKey_bit < alphabetSize; ++firstKey_bit) { // for every possible value of firstKey_bit, secondKey_bit
        SetBit(firstKey->element, firstKey_bit);

        // secondKey_bit and thirdKey_bit start from 0, so non-symmetric matrices should work
        for (size_t secondKey_bit = 0; secondKey_bit < alphabetSize; ++secondKey_bit) { // doesn't assumes 0 diagonal
            SetBit(secondKey->element, secondKey_bit);

            for (size_t thirdKey_bit = 0; thirdKey_bit < alphabetSize; ++thirdKey_bit) {
                if (DEBUG) printf("Insert firstKey_bit: %3zu, secondKey_bit: %3zu, thirdKey_bit: %3zu\n"
                                 , firstKey_bit, secondKey_bit, thirdKey_bit);
                SetBit(thirdKey->element, thirdKey_bit);
                // Originally used `getSetCostMedian()` here, but it involves a lot of overhead and we know we're only
                // using unambiguous elems, so we can just insert.
                std::get<0>(*toInsert) = tcm[  firstKey_bit * alphabetSize * alphabetSize
                                             + secondKey_bit * alphabetSize
                                             + thirdKey_bit ];
                // TODO: can I move the allocation out of `packedCharOr()`?
                if (std::get<1>(*toInsert) != NULL) {
                    std::free( std::get<1>(*toInsert) );
                }
                // TODO: Or'ing the median with third key should do the trick.
                std::get<1>(*toInsert) = packedCharOr(firstKey->element, secondKey->element, alphabetSize, 1);
                std::get<1>(*toInsert) = packedCharOr(firstKey->element, thirdKey->element, alphabetSize, 1);

                setValue(firstKey, secondKey, thirdKey, toInsert);
                ClearBit(std::get<1>(*toInsert), thirdKey_bit);
                ClearBit(thirdKey->element, thirdKey_bit);
            } // thirdKey_bit
            ClearBit(std::get<1>(*toInsert), secondKey_bit);
            ClearBit(secondKey->element, secondKey_bit);
        } // secondKey_bit
        ClearBit(firstKey->element, firstKey_bit);
        ClearBit(std::get<1>(*toInsert), firstKey_bit);
    }
    // Just to reiterate, getSetCostMedian() should allocate, so we should dealloc these.
    freeDCElem(firstKey);        // deallocate array
    std::free(firstKey);         // free pointer
    freeDCElem(secondKey);
    std::free(secondKey);
    freeDCElem(thirdKey);
    std::free(thirdKey);
    freeCostMedian_t(toInsert);
    delete toInsert;         // because generated with `new`
    // printf("finished initializing\n");
    // printf("freed keys\n");
}


void CostMatrix_3d::initializeTCM(const unsigned int* const inputBuffer)
{
    const auto bufferSize = alphabetSize * alphabetSize * sizeof(*tcm);
    tcm = (unsigned int*) std::malloc( bufferSize );
    std::memcpy( tcm, inputBuffer, bufferSize );
}


void CostMatrix_3d::setValue( const dcElement_t*  const first
                            , const dcElement_t*  const second
                            , const dcElement_t*  const third
                            , const costMedian_t* const toInsert
                            )
{
    // Making a deep copy of key & median here to help with memory management in calling fns.

    // Create a deep copy of the toInsert value to insert.
    const auto value = std::make_tuple( std::get<0>(*toInsert)
                                      , makePackedCharCopy( std::get<1>(*toInsert), alphabetSize, 1 )
                                      );

    // Create a new 3-tuple key to insert.
    const auto key = new keys_3d_t;

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

    // Copy the third element into key.
    const auto thirdElem       = new dcElement_t;
    std::get<2>(*key)          = *thirdElem;
    std::get<2>(*key).alphSize = alphabetSize;
    std::get<2>(*key).element  = makePackedCharCopy(third->element, alphabetSize, 1);

    // Add the copied key-value pair to the matrix.
    // This has to be a pair!
    // Clang is okay with make_tuple() or forward_as_tuple(), but gcc doesn't like it.
    myMatrix.insert(std::make_pair(*key, value));
    delete key;
    delete firstElem;
    delete secondElem;
    delete thirdElem;
}
