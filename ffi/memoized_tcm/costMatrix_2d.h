/** CostMatrix_2d object to provide for a memoizable cost lookup table. Table is indexed by two
 *  dcElement values, and returns an int, for the cost. In addition, an additional dcElement
 *  is passed in by reference, and the median value of the two input elements is placed there.
 *  The getCost function is designed to interface directly with C.
 *
 *  The key lookup is an ordered pair, so when looking up transition a -> b, a must go in as
 *  first in pair
 *
 *  WARNING: In the interest of speed this code does no "type checking" to make sure that the
 *  two passed deElements are of the same type, i.e. that they have the same alphabet length.
 *  Any such checks should be done exterior to this library.
 */

#ifndef _CostMatrix_2d_H
#define _CostMatrix_2d_H

#define DEBUG 0

// #include <cstdint>
// #include <pair>
#include <climits>
#include <cstdlib>
#include <unordered_map>

#ifdef __cplusplus
extern "C" {
#endif

#include "dynamicCharacterOperations.h"

/** Next three fns defined here to use on C side. */
costMatrix_p construct_CostMatrix_2d_C (size_t alphSize, int* tcm);
void destruct_CostMatrix_2d_C (costMatrix_p mytype);
int call_getSetCost_2d_C (costMatrix_p untyped_self, dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);
    // extern "C" costMatrix_p get_CostMatrix_Ptr_2d_C(costMatrix_p untyped_self);

#ifdef __cplusplus
}
#endif

// #include "CostMedPair.h"
typedef std::tuple<dcElement_t, dcElement_t>  keys_2d_t;
typedef std::tuple<int,         packedChar*>  costMedian_t;
typedef std::tuple<keys_2d_t,   costMedian_t> mapAccessTuple_2d_t;

/** Used to send 2d and 3d cost matrices through the C interface where they're tatically cast to the two matrix types. */
typedef void* costMatrix_p;


/** Allocate room for a costMedian_t. Assumes alphabetSize is already initialized. */
costMedian_t* allocCostMedian_t( size_t alphabetSize );


/** dealloc costMedian_t. */
void freeCostMedian_t( costMedian_t* toFree );


/** Allocate room for a keys_2d_t. */
keys_2d_t* allockeys_2d_t( size_t alphSize );


/** dealloc keys_2d_t. Calls various other free fns. */
void freekeys_2d_t( const keys_2d_t* toFree );


/** dealloc mapAccessTuple_2d_t. Calls various other free fns. */
void freemapAccessTuple_2d_t( const mapAccessTuple_2d_t* toFree );


/** Hashes two `dcElement`s, and returns an order-dependent hash value. In this case
 *  "order dependent" means that the order of the arrays within the `dcElement`s matter,
 *  and the order that the `dcElement`s are sent in also matters, as is necessary for a
 *  non-symmetric tcm.
 *
 *  First loops through each `dcElement` and combines all of the element values (recall that a
 *  `dcElement` has two fields, the second of which is the element, and is an array of `uint64_t`s)
 *  using two different seeds, then combines the two resulting values.
 */
struct KeyHash_2d {
    /** Following hash_combine code modified from here (seems to be based on Boost):
     *  http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
     */
    std::size_t hash_combine (const dcElement_t lhs, const dcElement_t rhs) const {
        std::size_t left_seed  = 3141592653; // PI used as arbitrarily random seed
        std::size_t right_seed = 2718281828; // E  used as arbitrarily random seed

        std::hash<uint64_t> hasher;
        auto elemArrCount = dcElemSize(lhs.alphSize);
        //printf("alphabetSize: %d\n", lhs.alphSize);
        //printf("elemArrCount: %d\n", elemArrCount);
        for (size_t i = 0; i < elemArrCount; i++) {
            left_seed  ^= hasher(lhs.element[i]) + 0x9e3779b9 + (left_seed  << 6) + (left_seed  >> 2);
            right_seed ^= hasher(rhs.element[i]) + 0x9e3779b9 + (right_seed << 6) + (right_seed >> 2);
        }
        left_seed ^= hasher(right_seed) + 0x9e3779b9 + (left_seed << 6) + (left_seed >> 2);
        //printf("%lu\n", left_seed);
        return left_seed;
    }

    std::size_t operator()(const keys_2d_t& k) const
    {
        // printf("operator hash ()\n");
        // printPackedChar(k.first.element, 1, k.first.alphSize);
        // printPackedChar(k.second.element, 1, k.second.alphSize);
        return hash_combine (std::get<0>(k), std::get<1>(k));
    }
};


struct KeyEqual_2d {
    // Return true if every `uint64_t` in lhs->element and rhs->element is equal, else false.
    bool operator()(const keys_2d_t& lhs, const keys_2d_t& rhs) const
    {
      // Assert that all key components share the same alphSize value
      if (   std::get<0>(lhs).alphSize  != std::get<0>(rhs).alphSize
          || std::get<0>(lhs).alphSize  != std::get<1>(lhs).alphSize
          || std::get<1>(lhs).alphSize != std::get<1>(rhs).alphSize) {
          return false;
      }

      //Assert that the left key elements match the right key elements
      auto elemArrWidth = dcElemSize(std::get<0>(lhs).alphSize);
        // printf("operator equal ()\n");
        // printPackedChar(std::get<0>(lhs).element, 1, std::get<0>(lhs).alphSize);
        // printPackedChar(std::get<0>(rhs).element, 1, std::get<0>(rhs).alphSize);
        // printPackedChar(std::get<1>(lhs).element, 1, std::get<1>(lhs).alphSize);
        // printPackedChar(std::get<1>(rhs).element, 1, std::get<1>(rhs).alphSize);
        for (size_t i = 0; i < elemArrWidth; i++) {
            if (std::get<0>(lhs).element[i] != std::get<0>(rhs).element[i]) {
                // printf("equal: false\n");
                return false;
            }
            if (std::get<1>(lhs).element[i] != std::get<1>(rhs).element[i]) {
                // printf("equal: false\n");
                return false;
            }
        }
        //printf("FAILED!!!!\n");
        return true;
    }
};


// typedef std::unordered_map<keys_2d_t, costMedian_t, KeyHash_2, KeyEqual_2>::const_iterator mapIterator;


class CostMatrix_2d
{
    public:

        /** Default constructor. Settings: alphabet size: 5, indel cost: 2, substitution cost: 1 */
        CostMatrix_2d();

        CostMatrix_2d(size_t alphSize, int* tcm);

        ~CostMatrix_2d();

        /** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median.
         *  @param ambElem is ambiguous input.
         *  @param nucleotide is unambiguous.
         *  @param median is used to return the calculated median value.
         *
         *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to
         *  look up ambElems, therefore we must loop over possible values of the ambElem
         *  and find the lowest cost median.
         *
         *  Public because gets called in CostMatrix_3d
         *
         *  Nota bene: Requires symmetric, if not metric, matrix. TODO: Is this true? If so fix it?
         */
        int findDistance(keys_2d_t* searchKey, dcElement_t* ambElem);

        /** Getter only for cost. Necessary for testing, to insure that particular
         *  key pair has, in fact, already been inserted into lookup table.
         */
        int getCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);

        /** Acts as both a setter and getter, mutating myMap.
         *
         *  Receives two dcElements and computes the transformation cost as well as
         *  the median for the two. Puts the median and alphabet size into retMedian,
         *  which must therefore by necessity be allocated elsewhere.
         *
         *  This function allocates _if necessary_. So freeing inputs after a call is necessary and will not
         *  cause invalid reads from the cost matrix.
         */
        int getSetCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);

    private:
        static constexpr int defaultExtraGapCostMetric[25] = {0, 1, 1, 1, 2,
                                                              1, 0, 1, 1, 2,
                                                              1, 1, 0, 1, 2,
                                                              1, 1, 1, 0, 2,
                                                              2, 2, 2, 2, 0};

        static constexpr int defaultDiscreteMetric[25]     = {0, 1, 1, 1, 1,
                                                              1, 0, 1, 1, 1,
                                                              1, 1, 0, 1, 1,
                                                              1, 1, 1, 0, 1,
                                                              1, 1, 1, 1, 0};

        static constexpr int defaultL1NormMetric[25]       = {0, 1, 2, 3, 4,
                                                              1, 0, 1, 2, 3,
                                                              2, 1, 0, 1, 2,
                                                              3, 2, 1, 0, 1,
                                                              4, 3, 2, 1, 0};


        std::unordered_map <keys_2d_t, costMedian_t, KeyHash_2d, KeyEqual_2d> myMatrix;

        std::unordered_map <keys_2d_t, costMedian_t, KeyHash_2d, KeyEqual_2d> hasher;

        const size_t alphabetSize;

        /** Always equal to:
         *    alphabetSize / sizeof ( packedChar ) + alphabetSize % sizeof(packedChar) ? 1 : 0
         *  Calculated once and stored for efficeincy.
         */
        const size_t elementSize;

        /** Stored unambiguous tcm, necessary to do first calls to findDistance() without having to rewrite
         *  findDistance() and computeCostMedian()
         */
        int* tcm;

        /** Takes in two `dcElement_t` and a `costMedian_t` and updates myMap to store the new values,
         *  with @{lhs, rhs} as a key, and @median as the value.
         *
         * Makes a deep copy of the arguments before inserting them into the map.
         */
         void setValue(const dcElement_t* const lhs, const dcElement_t* const rhs, const costMedian_t* const median);

        /** Takes in a pair of keys_2d_t (each of which is a single `dcElement`) and computes their lowest-cost median.
         *  Uses a Sankoff-like algorithm, where all bases are considered, and the lowest cost bases are included in the
         *  cost and median calculations. That means a base might appear in the median that is not present in either of
         *  the two elements being compared.
         */
        costMedian_t* computeCostMedian(keys_2d_t key);

        /** Takes in an initial TCM, which is actually just a row-major array, creates hash table of costs
         *  where cost is least cost between two elements, and medians, where median is union of characters.
         *
         *  Nota bene:
         *  Can only be called once this.alphabetSize has been set.
         */
        void initializeMatrix ();

        /** Takes an input buffer and assigns a malloc'ed copy to @tcm.
         *  Uses the @alphabetSize of the matrix to determine the required space.
         *  Because @alphabetSize is a const member, it will always be initialized
         *  before this call, making the allocation and copy safe so long as the
         *  input buffer is equal to or greater than @alphabetSize squared in
         *  length.
         */
        void initializeTCM(const int* const inputBuffer);

        /** Given a srcBuffer of a dynamic character element, this function
         *  creates a new buffer and copies the value of the provided dynamic
         *  character element buffer into the newly allocated buffer.
         *
         *  Uses the TCM's elementSize vairable to avoid recomputation of buffer
         *  size and reduces the number of function call parameters compared to
         *  the function makePackedCharCopy. Less noisy, slightly more efficient.
         *
         *  Useful to deep copy a packedChar* to a new pointer location.
         */
        packedChar* createCopyPackedChar(const packedChar* const srcBuffer);
};


#endif // CostMatrix_2d_H
