/** Class to hold a pair of cost, median, where a cost and median
 *  are set relative to two uint64_ts, and the median is a unint64_t that
 *  represents some overlap of those two inputs.
 *
 *  A uint64_t is a representation of some character alphabet value, such as 
 *  A, C, G, T.
 */

#ifndef _COSTMEDPAIR_H
#define _COSTMEDPAIR_H

class CostMedPair {
    
    private:
        int myCost;

        uint64_t myMedian;

    public:
            // default constructor
        CostMedPair();
            // constructor that sets a cost/median pair
        CostMedPair(int cost, uint_64_t median);

        ~CostMedPair();

        int getCost();

        uint64_t getMedian();

        void setCost(int cost);

        void setMedian(uint64_t median);
            // set one pair to another
        CostMedPair operator = (const CostMedPair& rhs);
};

#endif // COSTMEDPAIR_H