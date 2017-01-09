// unordered_map::find
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include "CostMatrix.h"


int main ()
{
  std::unordered_map<std::uint64_t,double> mymap = {
     {23,5.4},
     {24,6.1},
     {25,5.9} };

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
