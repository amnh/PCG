#include "costMatrix.h"

CostMatrix::CostMatrix() {
    alphabetSize = 2;

}

CostMatrix::CostMatrix(int alphSize, int* tcm) {
    alphabetSize = alphSize;
    setUpInitialMatrix(tcm);
}

CostMatrix::~CostMatrix() {

}

int CostMatrix::getCost(dcElement_t& left, dcElement_t& right, dcElement_t& retMedian) {
    keys_t toLookup (left, right);
    std::unordered_map<keys_t, costMedian_t, KeyHash, KeyEqual>::const_iterator found;
    int foundCost;

    found = myMatrix.find (toLookup);

    if ( found == myMatrix.end() ) {
        costMedian_t computedCostMed = computeCostMedian (toLookup);
        retMedian.element = computedCostMed.second.element;
        foundCost         = computedCostMed.first;

        setValue (toLookup, computedCostMed);
    } else {
            // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost = found->second.first;
        retMedian = found->second.second;
    }

    return foundCost;
}

costMedian_t CostMatrix::computeCostMedian(keys_t& key) {
    int curCost, minCost, median;
    for (uint64_t ambElem1 = 1; ambElem1 <= 31; ambElem1++) { // for every possible value of ambElem1, ambElem2
        for (uint64_t ambElem2 = 1; ambElem2 <= 31; ambElem2++) {
            curCost = 0; // don't actually need to initialize this
            minCost = INT_MAX;
            median  = 0;
            for (int nucleotide = 1; nucleotide <= alphabetSize; nucleotide++) {
                curCost = CM_distance (key) + // this isn't going to work: CM_distance() does the wrong thing
                          CM_distance (key);
                // now seemingly recreating logic in CM_distance(), but that was to get the cost for each
                // ambElem; now we're combining those costs get overall cost and median
                if (curCost < minCost) {
                    minCost = curCost;
                    median  = 1 << (nucleotide - 1); 
                } else if (curCost == minCost) {
                    median |= 1 << (nucleotide - 1); 
                }
            } // nucleotide

        } // ambElem2
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
costMedian_t* CostMatrix::CM_distance (keys_t& key, int* tcm) {

    packedChar_p key1      = key.first.element;
    packedChar_p key2      = key.second.element;
    size_t dynCharLen      = dcElemSize(alphabetSize);
    packedChar_p curMedian = (packedChar_p) calloc( dynCharLen, INT_WIDTH );
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    &toReturn->second       = &curMedian;
    int curMin             = INT_MAX;
    int curCost            = 0;

    for (size_t pos1 = 0; pos1 < alphabetSize; pos1++) {
        if ( TestBit(key1, pos1) ) { // if pos1 is possible value of key1
            for (size_t pos2; pos2 < alphabetSize; pos2++) {
                if ( TestBit(key2, pos2) ) { // pos1 in key1 and pos2 in key2
                    curCost = tcm[pos1 * alphabetSize + pos2];
                    if (curCost < curMin) {
                        curMin = curCost;
                        ClearAll (curMedian, dynCharLen);
                        SetBit (curMedian, pos1);
                        SetBit (curMedian, pos2);
                    } else if (curCost == curMin) {
                        SetBit (curMedian, pos1);
                        SetBit (curMedian, pos2);
                    }
                } // pos2 in key2
            } // pos2
        } // pos1 in key1
    } // key1
    free(key1);
    free(key2);
    //free(curMedian);
    toReturn->first = curMin;
    return toReturn;
}

void CostMatrix::setUpInitialMatrix (int* tcm) {
    std::pair<keys_t, costMedian_t> toInsert;
    toInsert.first.first.alphSize = toInsert.first.second.alphSize = 
                                    toInsert.second.second.alphSize = alphabetSize;
    for (size_t key1 = 1; key1 <= alphabetSize; key1 <<= 1) { // for every possible value of key1, key2
        for (size_t key2 = 1; key2 <= alphabetSize; key2 <<= 1) {
            SetBit(toInsert.first.first.element,  key1); 
            SetBit(toInsert.first.second.element, key2);
            toInsert.second.first          = tcm[(key1 - 1) * alphabetSize + key2 - 1];
            SetBit(toInsert.second.second.element, key1);
            SetBit(toInsert.second.second.element, key2); 
            if (!myMatrix.insert(toInsert) ) exit ("failed to insert.");
        } // key2
    }
}