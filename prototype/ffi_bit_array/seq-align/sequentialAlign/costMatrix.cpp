#include "costMatrix.h"

costMedian_t* allocCostMedian_t (size_t alphabetSize) {
    costMedian_t* toReturn;
    toReturn->second = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    return toReturn;
}

// CostMatrix::CostMatrix() {
//     alphabetSize = 2;

// }

CostMatrix::CostMatrix(size_t alphSize, int* tcm) {
    alphabetSize = alphSize;
    setUpInitialMatrix(tcm);
}

CostMatrix::~CostMatrix() {
    //TODO: actually write this

}

int CostMatrix::getCost(dcElement_t& left, dcElement_t& right, dcElement_t& retMedian) {
    keys_t toLookup (left, right);
    std::unordered_map<keys_t, costMedian_t, KeyHash, KeyEqual>::const_iterator found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        costMedian_t *computedCostMed = computeCostMedian(toLookup);
        foundCost                     = computedCostMed->first;
        retMedian.element             = computedCostMed->second;


        setValue (toLookup, computedCostMed);
    } else {
            // because in the next two lines, I get back a pair<keys, costMedian_t>
        foundCost         = found->second.first;
        retMedian.element = found->second.second;
    }

    return foundCost;
}

costMedian_t* CostMatrix::computeCostMedian(keys_t& key) {
    int curCost                   = 0; // don't actually need to initialize this
    dcElement_t *firstUnambigKey  = allocateDCElement(alphabetSize); // these will get set and used for lookup
    dcElement_t *secondUnambigKey = allocateDCElement(alphabetSize);
    packedChar_p curMedian;
    packedChar_p interimMedian;

    costMedian_t *toReturn; // not allocating second, because will be alloc'ed in curMedian
    toReturn->first               = INT_MAX;


    firstUnambigKey->alphSize  = alphabetSize;
    secondUnambigKey->alphSize = alphabetSize;
    keys_t searchKey;
    searchKey.first  = *firstUnambigKey;
    searchKey.second = *secondUnambigKey;

    std::unordered_map<keys_t, costMedian_t, KeyHash, KeyEqual>::const_iterator found;

    for (uint64_t posFirstKey = 1; posFirstKey <= alphabetSize; posFirstKey++) {
        if( TestBit(key.first.element, posFirstKey) ) {
            for (uint64_t posSecondKey = 1; posSecondKey <= alphabetSize; posSecondKey++) {
                if( TestBit(key.second.element, posSecondKey) ) {
                    SetBit(searchKey.first.element, posFirstKey);
                    SetBit(searchKey.second.element, posFirstKey);
                    // free(found.second);
                    found   = myMatrix.find(key);
                    curCost = found->second.first;
                    // now seemingly recreating logic in CM_distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost < toReturn->first) {
                        toReturn->first = curCost;
                        free(curMedian);
                        curMedian = found->second.second; // TODO: this is not making a copy, so need to fix it.
                    } else if (curCost == toReturn->first) {
                        interimMedian = packedCharOr(curMedian, found->second.second, alphabetSize);
                        free(curMedian);
                        curMedian = interimMedian;
                        free(interimMedian);
                    }
                } // if pos2 is set in second key
            } // if pos1 is set in first key
        } // posSecondKey
    } // posFirstKey
    freeDCElem(firstUnambigKey);
    freeDCElem(secondUnambigKey);
    return toReturn;
}

costMedian_t* CostMatrix::findDistance (keys_t& key, int* tcm) {

    packedChar_p key1      = key.first.element;
    packedChar_p key2      = key.second.element;
    size_t dynCharLen      = dcElemSize(alphabetSize);
    packedChar_p curMedian = (packedChar_p)  calloc( dynCharLen, INT_WIDTH );
    costMedian_t* toReturn = (costMedian_t*) malloc( sizeof(costMedian_t) );
    toReturn->second    = curMedian;
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

// TODO: deallocate here
void CostMatrix::setUpInitialMatrix (int* tcm) {
    std::pair<keys_t, costMedian_t> toInsert;
    toInsert.second.second = (uint64_t*) calloc(dcElemSize(alphabetSize), INT_WIDTH);
    toInsert.first.first.alphSize   = alphabetSize;
    toInsert.first.second.alphSize  = alphabetSize;
    for (size_t key1 = 1; key1 <= alphabetSize; key1 <<= 1) { // for every possible value of key1, key2
        for (size_t key2 = 1; key2 <= alphabetSize; key2 <<= 1) {
            SetBit(toInsert.first.first.element,  key1);
            SetBit(toInsert.first.second.element, key2);
            toInsert.second.first = tcm[(key1 - 1) * alphabetSize + key2 - 1];
            SetBit(toInsert.second.second, key1);
            SetBit(toInsert.second.second, key2);
            if ( !myMatrix.insert(toInsert).second ) {
                printf("failed to insert.\n");
                exit (1);
            }
        } // key2
    }
    freeDCElem(&toInsert.first.first);
    freeDCElem(&toInsert.first.second);
    free(toInsert.second.second);
}

void CostMatrix::setValue(keys_t key, costMedian_t *median) {
    if( !myMatrix.insert(std::make_pair(key, *median)).second ) {
        printf("failed to insert\n");
        exit(1);
    }
}