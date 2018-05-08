/** costMatrix object to provide for a memoizable cost lookup table. Table is indexed by two
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

#ifndef _COSTMATRIX_3D_H
#define _COSTMATRIX_3D_H

#define DEBUG 0

// #include <cstdint>
// #include <pair>
#include <climits>
#include <cstdlib>
#include <unordered_map>
#include "costMatrix_2d.hpp"


/********************* Next three fns defined here to use on C side. *********************/
#ifdef __cplusplus
extern "C" {
#endif

#include "dynamicCharacterOperations.h"

costMatrix_p construct_CostMatrix_C (size_t alphSize, unsigned int* tcm);

void destruct_CostMatrix_C (costMatrix_p mytype);

unsigned int call_costAndMedian2D_C ( costMatrix_p untyped_self
                                    , dcElement_t* first
                                    , dcElement_t* second
                                    , dcElement_t* retMedian
                                    );

unsigned int call_costAndMedian3D_C ( costMatrix_p untyped_self
                                    , dcElement_t* first
                                    , dcElement_t* second
                                    , dcElement_t* third
                                    , dcElement_t* retMedian
                                    );

// extern "C" costMatrix_p get_CostMatrix_2dPtr_C(costMatrix_p untyped_self);

#ifdef __cplusplus
}
#endif

/******************************** End of C interface fns ********************************/

typedef std::tuple<dcElement_t, dcElement_t, dcElement_t> keys_3d_t;
typedef std::tuple<keys_3d_t,   costMedian_t>             mapAccessTuple_3d_t;
// The stored cost & median type is defined in 2d matrix.


/** Allocate room for a keys_3d_t. */
keys_3d_t* allocKeys_3d_t (size_t alphSize);


/** dealloc keys_3d_t. Calls various other free fns. */
void freekeys_3d_t (const keys_3d_t* toFree);


/***************************************************************************************** *
 * Functions for 3d matrix lookup. Note that these functions rely on 2d lookup operations. *
 *******************************************************************************************/


/** Hashes three `dcElement`s, and returns an order-dependent hash value. In this case
 *  "order dependent" means that the order of the arrays within the `dcElement`s matters,
 *  and the order that the `dcElement`s are sent in also matters, as is necessary for a
 *  non-symmetric tcm.
 *
 *  First loops through each `dcElement` and combines all of the element values (recall that a
 *  `dcElement` has two fields, the second of which is the element, and is an array of `uint64_t`s)
 *  using two different seeds, then combines the two resulting values.
 */
struct KeyHash_3d
{
    /** Following hash_combine code modified from here (seems to be based on Boost):
     *  http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
     */
    std::size_t hash_combine ( const dcElement_t first
                             , const dcElement_t second
                             , const dcElement_t third ) const
    {
        std::size_t first_seed  = 3141592653; // π used as arbitrary random seed
        std::size_t second_seed = 2718281828; // e used as arbitrary random seed
        std::size_t third_seed  = 6022140857; // Avogadro's # used as arbitrary random seed

        std::hash<uint64_t> hasher;
        size_t elemArrCount = dcElemSize(first.alphSize);
        //printf("alphabetSize: %d\n", first.alphSize);
        //printf("elemArrCount: %d\n", elemArrCount);
        for (size_t i = 0; i < elemArrCount; i++) {
            first_seed  ^=   hasher(first.element[i])
                           + 0x9e3779b9
                           + (first_seed  << 6)
                           + (first_seed  >> 2);

            second_seed ^=   hasher(second.element[i])
                           + 0x9e3779b9
                           + (second_seed << 6)
                           + (second_seed >> 2);

            third_seed  ^=   hasher(third.element[i])
                           + 0x9e3779b9
                           + (third_seed << 6)
                           + (third_seed >> 2);
        }
        third_seed ^= hasher(first_seed) + hasher(second_seed) + 0x9e3779b9 + (third_seed << 6) + (third_seed >> 2);
        //printf("%lu\n", third_seed);
        return third_seed;
    }

    std::size_t operator()(const keys_3d_t& k) const
    {
        // printf("operator hash ()\n");
        // printPackedChar(k.first.element, 1, k.first.alphSize);
        // printPackedChar(k.second.element, 1, k.second.alphSize);
        return hash_combine( std::get<0>(k), std::get<1>(k), std::get<2>(k) );
    }
};


struct KeyEqual_3d
{
    // Return true if every `uint64_t` in key1->element and key2->element and key3->element
    // is equal, else false.
    bool operator()(const keys_3d_t& key1, const keys_3d_t& key2) const
    {
        // Assert that all key components share the same alphSize value.
        // There are two keys, with three values in each. They all need to have the same size alphabet.
        if (   std::get<0>(key1).alphSize != std::get<1>(key1).alphSize
            || std::get<1>(key1).alphSize != std::get<2>(key1).alphSize     // key1 is consistent
            || std::get<0>(key2).alphSize != std::get<1>(key2).alphSize
            || std::get<1>(key2).alphSize != std::get<2>(key2).alphSize     // key2 is consistent

            || std::get<0>(key1).alphSize != std::get<0>(key2).alphSize ) { // the keys match each other
          return false;
        }

        //Assert that the left key elements match the right key elements
        size_t elemArrWidth = dcElemSize(std::get<0>(key1).alphSize);
        // printf("operator equal ()\n");
        // printPackedChar(key1.first.element, 1, key1.first.alphSize);
        // printPackedChar(key2.first.element, 1, key2.first.alphSize);
        // printPackedChar(key1.second.element, 1, key1.second.alphSize);
        // printPackedChar(key2.second.element, 1, key2.second.alphSize);
        for (size_t i = 0; i < elemArrWidth; i++) {
            if (   std::get<0>(key1).element[i] != std::get<0>(key2).element[i]
                || std::get<1>(key1).element[i] != std::get<1>(key2).element[i]
                || std::get<2>(key1).element[i] != std::get<2>(key2).element[i] ) {
                // printf("equal: false\n");
                return false;
            }
        }
        //printf("FAILED!!!!\n");
        return true;
    }
};


typedef std::unordered_map<keys_3d_t, costMedian_t, KeyHash_3d, KeyEqual_3d>::const_iterator mapIterator_3d;


class CostMatrix_3d
{
    public:
        CostMatrix_3d();

        CostMatrix_3d( size_t alphSize, unsigned int* tcm );

        ~CostMatrix_3d();


        /** Returns the cost to transition between the *two* input elements and
         *  sets retMedian to be the median value between the *two* input
         *  elements.
         *
         *  If this is the first call to the function with the supplied inputs,
         *  then the cost and median will be calculated and the result with be
         *  internally cached.
         *
         *  If the pair of inputs have already been queried, the cached result
         *  is returned in constant time.
         *
         *  This function performs deep copies of the inputs _when necessary_.
         *  Freeing inputs after a call will _never_ cause invalid reads from
         *  the cost matrix.
         */
        unsigned int costAndMedian2D( dcElement_t* first
                                    , dcElement_t* second
                                    , dcElement_t* retMedian
                                      );


        /** Returns the cost to transition between the *three* input elements and
         *  sets retMedian to be the median value between the *two* input
         *  elements.
         *
         *  If this is the first call to the function with the supplied inputs,
         *  then the cost and median will be calculated and the result with be
         *  internally cached.
         *
         *  If the pair of inputs have already been queried, the cached result
         *  is returned in constant time.
         *  This function performs deep copies of the inputs _when necessary_.
         *  Freeing inputs after a call will _never_ cause invalid reads from
         *  the cost matrix.
         */
        unsigned int costAndMedian3D( dcElement_t* first
                                    , dcElement_t* second
                                    , dcElement_t* third
                                    , dcElement_t* retMedian
                                    );

    private:

        std::unordered_map <keys_3d_t, costMedian_t, KeyHash_3d, KeyEqual_3d> myMatrix;

        CostMatrix_2d* twoD_matrix;

        /** Takes in a `keys_3d_t` and a `costMedian_t` and updates myMap to store the new values,
         *  with @key as a key, and @median as the value.
         */
        void setValue( const dcElement_t* const first
                     , const dcElement_t* const second
                     , const dcElement_t* const third
                     , const costMedian_t* const median
                     );


        /** Takes in a pair of keys_3d_t (each of which is a single `dcElement`) and computes their
         *  lowest-cost median.
         *  Uses a Sankoff-like algorithm, where all bases are considered, and the lowest cost
         *  bases are included in the cost and median calculations. That means a base might appear
         *  in the median that is not present in either of the two elements being compared.
         */
        costMedian_t* computeCostMedian(keys_3d_t keys);

};

#endif // COSTMATRIX_3D_H
