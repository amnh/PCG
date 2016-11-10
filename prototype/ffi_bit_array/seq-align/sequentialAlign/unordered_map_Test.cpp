// unordered_map::find
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include "CostMatrix.h"

#define IDENTITY_COST 0
#define INDEL_COST    2
#define SUB_COST      1
#define ALPH_SIZE     5 // (includes gap)


int main ()
{
    std::unordered_map<std::uint64_t,double> mymap = {
       {23,5.4},
       {24,6.1},
       {25,5.9} };

    // This setup is for DNA tcm, but is a template for larger ones
    // 
    int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < tcm_total_len; i += ALPH_SIZE) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < ALPH_SIZE; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * ALPH_SIZE ) {
                // printf("i: %2zu, j: %2zu, cost: 0\n", i, j);
                tcm[i + j] = IDENTITY_COST;    // identity
            } else if (i == (tcm_total_len - ALPH_SIZE) || j == (ALPH_SIZE - 1)) {
                // printf("i: %2zu, j: %2zu, cost: 2\n", i, j);
                tcm[i + j] = INDEL_COST;      // indel cost
            } else {
                // printf("i: %2zu, j: %2zu, cost: 1\n", i, j);
                tcm[i + j] = SUB_COST;        // sub cost
            }
         }
    }

    std::string input;
    std::cout << "who? ";
    getline (std::cin,input);
    uint64_t inVal;
    std::stringstream inter(input);
    //inter << input;
    inter >> inVal;
    std::unordered_map<std::uint64_t,double>::const_iterator got = mymap.find (inVal);

    if ( got == mymap.end() )
      std::cout << "not found";
    else 
      std::cout << got->first << " is " << got->second;

    std::cout << std::endl;

    return 0;
}
