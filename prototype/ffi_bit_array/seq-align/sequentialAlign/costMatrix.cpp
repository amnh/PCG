#include "costMatrix.h"

CostMatrix::CostMatrix() {
    alphabetSize = 2;

}

CostMatrix::CostMatrix(int alphSz) {
    alphabetSize = alphSz;
    myMatrix 
}

CostMatrix::~CostMatrix() {}

int getCost(dcElement_t& left, dcElement_t& right, dcElement_t& retMedian) {
    keys toLookup (left, right);  
    std::unordered_map<keys, costMedian, KeyHash, KeyEqual>::const_iterator found;
    int foundCost;

    found = myMap.find (toLookup);

    if ( found == myMatrix.end() ) {
        retMedian->element = computeCostMedian (toLookup);
        foundCost          = computedCostMed->first;

        setValue (toLookup, computedCostMed);
    } else {
        foundCost = found->first;
        retMedian = found->second;
    }

    return foundCost;
}

costMedian computeCostMedian(keys& key) {
    for (SEQT ambElem1 = 1; ambElem1 <= 31; ambElem1++) { // for every possible value of ambElem1, ambElem2
        for (SEQT ambElem2 = 1; ambElem2 <= 31; ambElem2++) {
                curCost = 0; // don't actually need to initialize this
                minCost = INT_MAX; 
                median  = 0;
                for (int nucleotide = 1; nucleotide <= alphSize; nucleotide++) {
                    curCost = distance (key) +
                                distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem2);
                    // now seemingly recreating logic in distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost < minCost) {
                        minCost = curCost;
                        median  = 1 << (nucleotide - 1); 
                    } else if (curCost2d == minCost2d) {
                        median |= 1 << (nucleotide - 1); 
                    }
                } // nucleotide
                            
            } // ambElem2
            // printf("ambElem1:  %2hhu,   ambElem2: %2hhu\n", ambElem1, ambElem2);
            // printf("median: %2d,   min:   %2d\n", median2d, minCost2d);
            cm_set_cost_2d   (ambElem1, ambElem2, minCost2d, (cost_matrices_2d_p) retMtx);
            cm_set_median_2d (ambElem1, ambElem2, median2d,  (cost_matrices_2d_p) retMtx);
            // if (power_2(ambElem1) && power_2(ambElem2)) {
            //     printf("2d    seq1: %d,    seq2: %d,    cost: %d,    median: %2d\n", ambElem1, ambElem2, minCost2d, median2d);
            // }
    } // ambElem1
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
int distance (keys& key) {

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
