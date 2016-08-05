#ifndef DYNAMIC_CHARACTER
#define DYNAMIC_CHARACTER

#include <boost/unordered_map.hpp>
#import  <pair>
#include <vector>

#include "../dynamic_character.h"

typedef pair<dynChar_t, double> returnType;
typedef boost::unordered_map<uint64_t, returnType> Matrix;

class MemoizedTCM {
        
    public:
        MemoizedTCM( double* costArray, size_t alphSize ); // constructor
        
        /** Find and return cost/overlap pair from cost matrix.
         *  If cost doesn't exist, calculates it and stores
         *  that key/value in matrix before returing the pair.
         *  Does no checking to guarantee charA & charB are a valid
         *  pair of inputs.
         */
        returnType getCost( const dynChar_t* const charA, const dynChar_t* const charB );


 
    private:
        Matrix costMatrix;

        /** Writes calculated cost into cost matrix at location hashVal. */
        void assignValue( uint64_t hashVal, double cost );

        /** Finds overlap between charA and charB. If no overlap, returns
         *  calculated cost by finding least cost of each pairing of dynamic character elements.
         *  Returns a hashed value, to find cost in memoized matrix, as well as the cost.
         */
        returnType calculateCost( dynChar_t const &charA, dynChar_t const &charB );
};

#endif /* MEMOIZED_TCM */