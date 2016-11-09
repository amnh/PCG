#include costMedPair.h


    // default constructor
CostMedPair() {
    myCost   = 0;
    myMedian = 0;
}

CostMedPair(int cost, uint64_t median) {
    myCost   = cost;
    myMedian = median;
}

~CostMedPair();

int getCost() {
    return myCost;
}

uint64_t getMedian() {
    return myMedian;
}

void setCost(int cost) {
    myCost = cost;
}

void setMedian(uint64_t median) {
    myMedian = median;
}

CostMedPair operator = (const CostMedPair& rhs) {
    myCost   = rhs.myCost;
    myMedian = rhs.myMedian;
}
