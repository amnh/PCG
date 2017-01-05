#include "costMatrix.h"

CostMatrix::CostMatrix() {
    alphabetSize = 2;
}

CostMatrix::CostMatrix(int alphSz) {
    alphabetSize = alphSz;
}

CostMatrix::~CostMatrix() {}

int getCost(uint64_t left, uint64_t right, uint64_t& retMedian) {
    keys toLookup (left, right);  
    costMedian computedCostMed;   // value returned from computeCostMedian()
    std::unordered_map<keys, costMedian>::const_iterator found;
    int foundCost;

    found = myMap.find (toLookup);

    if ( found == myMatrix.end() ) {
        computedCostMed = computeCostMedian(toLookup);
        foundCost       = computedCostMed->first;
        retMedian       = computedCostMed->second;

        setValue (toLookup, computedCostMed);
    } else {
        foundCost = found->first;
        retMedian = found->second;
    }

    return foundCost;
}

costMedian computeCostMedian(keys key) {
    
}

/** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median. 
 *  @param ambElem is ambiguous input.
 *  @param nucleotide is unambiguous.
 *  @param median is used to return the calculated median value.
 *
 *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to 
 *  look up ambElems, therefore we must loop over possible values of the ambElem
 *  and find the lowest cost median.
 *
 *  Requires symmetric, if not metric, matrix.
 */
int distance (keys key) {

    int min     = INT_MAX;
    int curCost = 0;

    for (size_t pos = 0; pos < alphSize; pos++) {

        if ( setbit(key->first) ) { // if pos is possible value of first
            if ()
            curCost = tcm[pos * alphSize + nucleotide - 1];
            if (curCost < min) {
                min = curCost;
            } 
        }
    }
    // printf("ambElem:   %2d,   nuc:   %2d,   min: %2d\n", ambElem, nucleotide, min);
    return min;
}

void computeInitialMatrix (size_t alphSize) {
    for (size_t ambElem1 = 1; ambElem1 <= 31; ambElem1++) { // for every possible value of ambElem1, ambElem2, ambElem3
        for (size_t ambElem2 = 1; ambElem2 <= 31; ambElem2++) {
                curCost2d = 0; // don't actually need to do this
                minCost2d = INT_MAX; 
                // maxCost   = 0;
                median2d  = 0;
                median1   = median2  = 0;
                for (size_t nucleotide = 1; nucleotide <= alphabetSize; nucleotide++) {
                    // TODO: if we do maxCost, then we should find individual max's for each distance below?
                    curCost2d = distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem1) +
                                distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem2);
                    // now seemingly recreating logic in distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost2d < minCost2d) {
                        minCost2d = curCost2d;
                        median2d  = 1 << (nucleotide - 1); // median1 | median2;
                    } else if (curCost2d == minCost2d) {
                        median2d |= 1 << (nucleotide - 1); // median1 | median2;
                    }
                } // nucleotide
                
            
            cm_set_cost_2d   (ambElem1, ambElem2, minCost2d, (cost_matrices_2d_p) retMtx);
            cm_set_median_2d (ambElem1, ambElem2, median2d,  (cost_matrices_2d_p) retMtx);
            // if (power_2(ambElem1) && power_2(ambElem2)) {
            //     printf("2d    seq1: %d,    seq2: %d,    cost: %d,    median: %2d\n", ambElem1, ambElem2, minCost2d, median2d);
            // }
        } // ambElem2
    }
}

template <typename T>
void hash_combine(size_t& seed, const T* v, size_t curLoc, size_t length) {
    std::hash<T> hasher;
    while (length > curLoc) {
        seed ^= hasher(v[curLoc]) + 0x9e3779b9 + (seed<<6) + (seed>>2);
        curLoc++;
    }
}

template <typename T>
void hash_combine(size_t& seed, T lhs, T rhs) {
    std::hash<T> hasher;
    seed ^= hasher(lhs) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    seed ^= hasher(rhs) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

