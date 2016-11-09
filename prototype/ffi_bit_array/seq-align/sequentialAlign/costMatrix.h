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
#include <string>
#include <unordered_map>

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
// #include "CostMedPair.h"

typedef std::pair<dcElement_t, dcElement_t> keys;
typedef std::pair<int, dcElement_t> costMedian;

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
    hash_combine(dcElement_t& lhs, dcElement_t& rhs) {
        std::size_t left_seed  = 3141592653;
        std::size_t right_seed = 2718281828;
        
        std::hash<uint64_t> hasher;
        int numElems = lhs->alphSize / 64 + 1;
        for (int i = 0; i < numElems; i++) {
            left_seed  ^= hasher(lhs->element[i]) + 0x9e3779b9 + (left_seed  << 6) + (left_seed  >> 2);
            right_seed ^= hasher(rhs->element[i]) + 0x9e3779b9 + (right_seed << 6) + (right_seed >> 2);
        }
        left_seed ^= hasher(right_seed) + 0x9e3779b9 + (left_seed << 6) + (left_seed >> 2);
        return left_seed;
    }

    std::size_t operator()(const keys& k) const
    {
        return hash_combine (k.first, k.second);
    }
};

struct KeyEqual {
    // Return true if every `uint64_t` in lhs->element and rhs->element is equal, else false.
    bool operator()(const keys& lhs, const keys& rhs) const
    {
        int numElems = lhs->alphSize / 64 + 1;
        for (int i = 0; i < numElems; i++) {
            if (lhs->element != rhs->element) {
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

        CostMatrix(int alphSz);

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
         *  have only a single `keys` argument and return a `costMedian`.
         */ 
        int getCost(dcElement_t& left, dcElement_t& right, dcElement_t& retMedian);

    private:
        std::unordered_map <keys, costMedian, KeyHash, KeyEqual> myMatrix;
        std::unordered_map::hasher

        // not positive we need this, as the alphabet size is alway stored in the incoming dcElements
        int alphabetSize;

        /** Takes in a `keys` and a `costMedian` and updates myMap to store the new values, 
         *  with @key as a key, and @median as the value.
         */
        void setValue(keys key, costMedian median);

        /** Takes in a pair of keys (each of which is a single `dcElement`) and computes their lowest-cost median. */
        costMedian computeCostMedian(keys key);


};

#endif // COSTMATRIX_H