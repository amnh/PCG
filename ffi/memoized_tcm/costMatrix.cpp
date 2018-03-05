#include <cstring> //for memcpy;
#include <inttypes.h>

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

costMatrix_p construct_CostMatrix_C(size_t alphSize, int* tcm)
{
    return new CostMatrix(alphSize, tcm);
}


void destruct_CostMatrix_C(costMatrix_p untyped_self)
{
    delete static_cast<CostMatrix*> (untyped_self);
}


int call_getSetCost_C( costMatrix_p untyped_self
                     , dcElement_t* left
                     , dcElement_t* right
                     , dcElement_t* retMedian
                     )
{

    CostMatrix* thisMtx = static_cast<CostMatrix*> (untyped_self);
    return thisMtx->getSetCostMedian(left, right, retMedian);
}


// costMatrix_p get_CostMatrixPtr_C(costMatrix_p untyped_self);


costMedian_t* allocCostMedian_t (size_t alphabetSize)
{
    auto toReturn = new costMedian_t;
    std::get<0>(*toReturn) = 0;
    std::get<1>(*toReturn) = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    return toReturn;
}


void freeCostMedian_t (costMedian_t* toFree)
{
    packedChar* medianValue = std::get<1>(*toFree);
    if (medianValue == NULL)
      return;

    free(medianValue);
    std::get<1>(*toFree) = NULL;
}


keys_t* allocKeys_t (size_t alphabetSize)
{
    auto toReturn = new keys_t;

    std::get<0>(*toReturn) = *allocateDCElement(alphabetSize);
    std::get<1>(*toReturn) = *allocateDCElement(alphabetSize);

    std::get<0>(*toReturn).alphSize = std::get<1>(*toReturn).alphSize                            = alphabetSize;

    return toReturn;
}

// TODO: since keys_t is Pair<dcElement_t, dcElement_t>, there are no pointers, and nothing
// to free?? How is this right?
void freeKeys_t ( const keys_t* toFree )
{
    freeDCElem(&std::get<0>(*toFree));
    freeDCElem(&std::get<1>(*toFree));
    // C++ memory Manager will free toFree
}


CostMatrix::CostMatrix()
{
    alphabetSize = 5;
    int inTcm[25] = {0, 1, 1, 1, 2,  1, 0, 1, 1, 2,  1, 1, 0, 1, 2,  1, 1, 1, 0, 2,  2, 2, 2, 2, 0};
    tcm = new int[25];
    memcpy(tcm, inTcm, alphabetSize * alphabetSize * sizeof(int));
    initializeMatrix();
}


CostMatrix::CostMatrix(size_t alphSize, int* inTcm)
{
    alphabetSize = alphSize;
    elementSize  = alphabetSize % sizeof(packedChar);
    tcm = new int[alphabetSize * alphabetSize];
    memcpy(tcm, inTcm, alphabetSize * alphabetSize * sizeof(int));
    initializeMatrix();
}


CostMatrix::~CostMatrix()
{
    // We occasionally invalid free pointers that were already freed with this loop. TODO: Occasionally?
    /*
    for ( auto& thing: myMatrix ) {
    // for ( mapIterator thing = myMatrix.begin(); thing != myMatrix.end(); thing++ ) {
        //freeCostMedian_t(&std::get<1>(thing));
        // TODO: since keys_t is tuple<dcElement_t, dcElement_t>, there are no pointers, and nothing
        // to free?? How is this right? Anyway, skipping next line.
        // freeKeys_t( &std::get<0>(thing) );
    }
    */
    myMatrix.clear();
    hasher.clear();
}


int CostMatrix::getCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian)
{
    keys_t toLookup;
    std::get<0>(toLookup) = *left;
    std::get<1>(toLookup) = *right;
    auto foundCost{0};

    auto found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        foundCost          = std::get<0>(std::get<1>(*found));
    if(retMedian->element != NULL)
       free(retMedian->element);
        retMedian->element = std::get<1>(std::get<1>(*found));
    }

    return foundCost;
}


int CostMatrix::getSetCostMedian( dcElement_t* left
                                , dcElement_t* right
                                , dcElement_t* retMedian
                                )
{
    // not using allocKeys_t because we're making a copy of the packed characters _if we need to_
    auto toLookup          = new keys_t;
    std::get<0>(*toLookup) = *left;
    std::get<1>(*toLookup) = *right;
    auto found = myMatrix.find(*toLookup);
    auto foundCost{0};

    if(DEBUG) {
        printf("1st: {%zu}: %" PRIu64 "\n", std::get<0>(*toLookup).alphSize, *std::get<0>(*toLookup).element ), fflush(stdout);
        printf("2nd: {%zu}: %" PRIu64 "\n", std::get<1>(*toLookup).alphSize, *std::get<1>(*toLookup).element), fflush(stdout);
    }

    if ( found == myMatrix.end() ) {
        if(DEBUG) printf("\ngetSetCost didn't find %" PRIu64 " %" PRIu64 ".\n", left->element[0], right->element[0]);

        auto computedCostMed = computeCostMedian(*toLookup);
        //costMedian_t* computedCostMed = computeCostMedianFitchy(*toLookup);

        if(DEBUG) printf("computed cost, median: %2i %" PRIu64 "\n", std::get<0>(*computedCostMed), std::get<1>(*computedCostMed)[0]);

        foundCost          = std::get<0>(*computedCostMed);

    if(retMedian->element != NULL)
        free(retMedian->element);
        retMedian->element = makePackedCharCopy( std::get<1>(*computedCostMed), alphabetSize, 1 );

        // Was using toLookup here, but the memory allocation was getting confusing.
        auto insertKey = new keys_t;
        // Can't use allocateDCElement here, because makePackedCharCopy() allocates.
        std::get<0>(*insertKey)          = *(new dcElement_t);
        std::get<0>(*insertKey).alphSize = left->alphSize;
        std::get<0>(*insertKey).element  = makePackedCharCopy( left->element , alphabetSize, 1 );

        std::get<1>(*insertKey)          = *(new dcElement_t);
        std::get<1>(*insertKey).alphSize = right->alphSize;
        std::get<1>(*insertKey).element  = makePackedCharCopy( right->element, alphabetSize, 1 );

        setValue(insertKey, computedCostMed);
        // freeKeys_t(insertKey); Don't want to free this because it gets copied by ref into the map.
    } else {
        // because in the next two lines, I get back a tuple<keys, costMedian_t>
        foundCost          = std::get<0>(std::get<1>(*found));
        if(retMedian->element != NULL)
            free(retMedian->element);
    retMedian->element = makePackedCharCopy( std::get<1>(std::get<1>(*found)), alphabetSize, 1 );
    }

    return foundCost;
}


costMedian_t* CostMatrix::computeCostMedian(keys_t keys)
{
    auto curCost{INT_MAX},
         minCost{INT_MAX};

    auto elemArrLen = dcElemSize(alphabetSize);
//    packedChar*   median     = (packedChar*) calloc( elemArrLen, sizeof(uint64_t) );
    auto firstKey  = &std::get<0>(keys);
    auto secondKey = &std::get<1>(keys);
    auto toReturn  = new costMedian_t;   // array is alloc'ed above

    // TODO: I need the alloc here, `new` won't work. Why?
    packedChar* curMedian = (packedChar*) calloc(elemArrLen, INT_WIDTH);  // don't free, it's going into toReturn
    auto searchKey        = allocKeys_t(alphabetSize);
    auto singleNucleotide = &std::get<1>(*searchKey);

    if(DEBUG) {
        for ( auto& thing: myMatrix ) {
            printf( "%" PRIu64 " %" PRIu64 "\n"
                  , std::get<0>(std::get<0>(thing)).element[0]
                  , std::get<1>(std::get<0>(thing)).element[0]);
            printElemBits( &std::get<0>(std::get<0>(thing)) );
            printElemBits( &std::get<1>(std::get<0>(thing)) );
        }
    }

    for (size_t curNucleotideIdx = 0; curNucleotideIdx < alphabetSize; ++curNucleotideIdx) {
        SetBit(singleNucleotide->element, curNucleotideIdx);
        curCost = findDistance(searchKey, firstKey)
                + findDistance(searchKey, secondKey);

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
        }
    else if (curCost == minCost) {
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

    freeKeys_t(searchKey);
    delete searchKey;

    return toReturn;
}


/** Find minimum substitution cost from one nucleotide (searchKey->second) to ambElem.
 *  Does so by setting a bit in searchKey->first, then doing a lookup in the cost matrix.
 */
int CostMatrix::findDistance (keys_t* searchKey, dcElement_t* ambElem)
{
    mapIterator found;
    auto minCost{INT_MAX},
         curCost{INT_MAX};
    size_t unambElemIdx;

    for (size_t pos = 0; pos < alphabetSize; pos++) {
        if (TestBit(ambElem->element, pos)) {

            SetBit( std::get<0>(*searchKey).element, pos );
            found = myMatrix.find(*searchKey);
            if (found == myMatrix.end()) {
                // do unambiguous calculation here
                if( !isAmbiguous(ambElem->element, dcElemSize(alphabetSize)) ) {
                    unambElemIdx = 0;
                    while( unambElemIdx < alphabetSize && !TestBit(std::get<1>(*searchKey).element, unambElemIdx) ) {
                        unambElemIdx++;
                    }
                    curCost = tcm[pos * alphabetSize + unambElemIdx];
                    // printf("\n--findDistance-- \n    ambElemIdx: %zu, nucleotide: %zu, cost: %d\n", unambElemIdx, pos, curCost);
                }
        else {
                    printf("Something went wrong in the memoized cost matrix.\n");
                    printf("missing key: %" PRIu64 " %" PRIu64 "\n", *std::get<0>(*searchKey).element, *std::get<1>(*searchKey).element);
                    exit(1);
                }
            }
        else {  // We found the memoized cost for the elements in the TCM.
                curCost = std::get<0>(std::get<1>(*found));
            }
            if (curCost < minCost) {
                minCost = curCost;
            }
            ClearBit(std::get<0>(*searchKey).element, pos);
        }
    }

    if (DEBUG) {
        printf("distance ambElem: %" PRIu64 ", nucleotide: %" PRIu64 "\n", ambElem->element[0], *std::get<1>(*searchKey).element);
        printf("cost: %i\n", minCost);
    }

    return minCost;
}


void CostMatrix::initializeMatrix()
{
    auto firstKey  = allocateDCElement( alphabetSize );
    auto secondKey = allocateDCElement( alphabetSize );
    auto retMedian = allocateDCElement( alphabetSize );

    for (size_t key1 = 0; key1 < alphabetSize; key1++) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);

        // key2 starts from 0, so non-symmetric matrices should work
        for (size_t key2 = 0; key2 < alphabetSize; key2++) { // doesn't assumes 0 diagonal
            SetBit(secondKey->element, key2);
            CostMatrix::getSetCostMedian(firstKey, secondKey, retMedian);

             ClearBit(secondKey->element, key2);
        } // key2
        ClearBit(firstKey->element, key1);
    }
    freeDCElem( firstKey );
    freeDCElem( secondKey );
    freeDCElem( retMedian );
    // printf("finished initializing\n");
    // printf("freed keys\n");
}


void CostMatrix::setValue(const keys_t* const key, const costMedian_t* const median)
{
    // This has to be a pair. Clang is okay with make_tuple() or forward_as_tuple(), but gcc doesn't like it.
    // TODO: We might want a deep copy of key & median here to help with mempory management.
    //std::tuple<int,         packedChar*>  costMedian_t
  /*
    auto medianCopy = new costMedian_t;
    auto packedCopy = malloc(elementSize * sizeof(packedChar));
    memcpy(packedCopy, std::get<1>(median), elementSize);
    std::get<0>(medianCopy) = std::get<0>(median);
    std::get<1>(medianCopy) = packedCopy;
  */
    myMatrix.insert(std::make_pair(*key, *median));
}
