/** costMatrix object to provide for a memoizable cost lookup table. Table is indexed by two
 *  dcElement values, and returns an int, for the cost. In addition, an additional dcElement 
 *  is passed in by reference, and the median value of the two input elements is placed there.
 *  The getCost function is designed to interface directly with C.
 *
 *  WARNING: In the interest of speed this code does no "type checking" to make sure that the 
 *  two passed deElements are of the same type, i.e. that they have the same alphabet length. 
 *  Any such checks should be done exterior to this library.
 */

#ifndef _COSTMATRIX_H
#define _COSTMATRIX_H

// #include <cstdint>
// #include <pair>
#include <climits>
#include <stdlib.h>
#include <string> // TODO: remember to delete this
#include <unordered_map>

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
// #include "CostMedPair.h"

typedef std::pair<dcElement_t, dcElement_t> keys_t;
typedef std::pair<int, dcElement_t> costMedian_t;

/** Hashes two `dcElement`s, and returns an order-dependent hash value. In this case
 *  "order dependent" means that the order of the arrays within the `dcElement`s matter,
 *  and the order that the `dcElement`s are sent in also matters, as is necessary for a
 *  non-symmetric tcm.
 *
 *  First loops through each `dcElement` and combines all of the element values (recall that a
 *  `dcElement` has two fields, the second of which is the element, and is an array of `uint64_t`s)
 *  using two different seeds, then combines the two resulting values.
 */
struct KeyHash {
    /** Following hash_combine code modified from here (seems to be based on Boost):
     *  http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
     */
    std::size_t hash_combine (const dcElement_t& lhs, const dcElement_t& rhs) const {
        std::size_t left_seed  = 3141592653;
        std::size_t right_seed = 2718281828;
        
        std::hash<uint64_t> hasher;
        int numElems = lhs.alphSize / 64 + 1;
        for (int i = 0; i < numElems; i++) {
            left_seed  ^= hasher(lhs.element[i]) + 0x9e3779b9 + (left_seed  << 6) + (left_seed  >> 2);
            right_seed ^= hasher(rhs.element[i]) + 0x9e3779b9 + (right_seed << 6) + (right_seed >> 2);
        }
        left_seed ^= hasher(right_seed) + 0x9e3779b9 + (left_seed << 6) + (left_seed >> 2);
        return left_seed;
    }

    std::size_t operator()(const keys_t& k) const
    {
        return hash_combine (k.first, k.second);
    }
};

struct KeyEqual {
    // Return true if every `uint64_t` in lhs->element and rhs->element is equal, else false.
    bool operator()(const keys_t& lhs, const keys_t& rhs) const
    {
        int numElems = 1 << (lhs.first.alphSize - 1); // assume that alphabet sizes for all four dcElements are the same
        for (size_t i = 0; i < numElems; i++) {
            if (lhs.first.element[i] != rhs.first.element[i]) {
                return false;
            }
            if (lhs.second.element[i] != rhs.second.element[i]) {
                return false;
            }
        }
        return true;
    }
};

class CostMatrix 
{
    public:
        CostMatrix();

        CostMatrix(int alphSz, int* tcm);

        ~CostMatrix();  // TODO: actually write this.

        /** This is the only way to interact with this class. Acts as both a setter
         *  and getter, mutating myMap.
         * 
         *  Receives two dcElements and computes the transformation cost as well as 
         *  the median for the two. Puts the median and alphabet size into retMedian,
         *  which must therefore by necessity be allocated elsewhere. 
         *
         *  This fn will interact with C, so has to return cost primitive int, 
         *  and median will be returned by reference. At least that's my naive 
         *  assumption. Maybe after I work on the C wrapper I'll figure out how to
         *  have only a single `keys_t` argument and return a `costMedian_t`.
         */ 
        int getCost(dcElement_t& left, dcElement_t& right, dcElement_t& retMedian);

    private:
        std::unordered_map <keys_t, costMedian_t, KeyHash, KeyEqual> myMatrix;
        
        std::unordered_map <keys_t, costMedian_t, KeyHash, KeyEqual> hasher;

        // not positive we need this, as the alphabet size is always stored in the incoming dcElements
        int alphabetSize;

        /** Takes in a `keys_t` and a `costMedian_t` and updates myMap to store the new values, 
         *  with @key as a key, and @median as the value.
         */
        void setValue(keys_t key, costMedian_t median);

        /** Takes in a pair of keys_t (each of which is a single `dcElement`) and computes their lowest-cost median. */
        costMedian_t computeCostMedian(keys_t& key);

        int CM_distance (keys_t& key);

        // can only be called once this.alphabetSize has been set.
        void setUpInitialMatrix (int* tcm);

};

#endif // COSTMATRIX_H