#ifndef _COSTMATRIX_H
#define _COSTMATRIX_H

// #include <cstdint>
// #include <pair>
#include <string>
#include <unordered_map>

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
// #include "CostMedPair.h"


typedef std::pair<uint64_t, uint64_t> keys;
typedef std::pair<int, uint64_t> costMedian;

class CostMatrix 
{
    public:
        CostMatrix();

        CostMatrix(int alphSz);

        ~CostMatrix();
            // this fn will interact with C, so has to return cost primitive int, 
            // and median will be returned by reference
        int getCost(uint64_t left, uint64_t right, uint64_t& retMedian);

    private:
        std::unordered_map <keys, costMedian> myMatrix;
        std::unordered_map::hasher

        int alphabetSize;

        void setValue(keys key, costMedian cm);

        costMedian computeCostMedian(keys key);

        /** Following hash_combine code modified from here (seems to be based on Boost):
         *  http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
         */

        template <typename T>
        inline void hash_combine(size_t& seed, const T* v, size_t curLoc, size_t length);

        template <typename T>
        inline void hash_combine(size_t& seed, T lhs, T rhs);
};

#endif // COSTMATRIX_H